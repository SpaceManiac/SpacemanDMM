use super::*;
use either::Either;
use gif::DisposalMethod;

static NO_TINT: [u8; 4] = [0xff, 0xff, 0xff, 0xff];

/// Used to render an IconFile to a .gif/.png easily.
#[derive(Debug)]
pub struct IconRenderer<'a> {
    /// The IconFile we render from.
    source: &'a IconFile,
}

/// [`IconRenderer::render`] will return this to indicate if it wrote to the stream using
/// [`gif::Encoder`] or `[`png::Encoder`].
#[derive(Debug, Clone, Copy)]
pub enum RenderType {
    Png,
    Gif,
}

#[derive(Debug)]
pub struct RenderStateGuard<'a> {
    pub render_type: RenderType,
    renderer: &'a IconRenderer<'a>,
    state: &'a State,
}

impl<'a> RenderStateGuard<'a> {
    pub fn render<W: std::io::Write>(self, target: W) -> io::Result<()> {
        match self.render_type {
            RenderType::Png => self.renderer.render_to_png(self.state, target),
            RenderType::Gif => self.renderer.render_gif(self.state, target),
        }
    }
}

/// Public API
impl<'a> IconRenderer<'a> {
    pub fn new(source: &'a IconFile) -> Self {
        Self { source }
    }

    /// Renders with either [`gif::Encoder`] or [`png::Encoder`] depending on whether the icon state is animated
    /// or not.
    /// Returns a [`RenderType`] to help you determine how to treat the written data.
    pub fn prepare_render(&self, icon_state: &StateIndex) -> io::Result<RenderStateGuard> {
        self.prepare_render_state(self.source.get_icon_state(icon_state)?)
    }

    /// This is here so that duplicate icon states can be handled by not relying on the btreemap
    /// of state names in [`Metadata`].
    pub fn prepare_render_state(&'a self, icon_state: &'a State) -> io::Result<RenderStateGuard> {
        match icon_state.is_animated() {
            false => Ok(RenderStateGuard {
                renderer: self,
                state: icon_state,
                render_type: RenderType::Png,
            }),
            true => Ok(RenderStateGuard {
                renderer: self,
                state: icon_state,
                render_type: RenderType::Gif,
            }),
        }
    }

    /// Instead of writing to a file, this gives a Vec<Image> of each frame/dir as it would be composited
    /// for a file.
    pub fn render_to_images(&self, icon_state: &StateIndex) -> io::Result<Vec<Image>> {
        let state = self.source.get_icon_state(icon_state)?;
        Ok(self.render_frames(state))
    }
}

/// Private helpers
impl<'a> IconRenderer<'a> {
    /// Helper for render_to_images- not used for render_gif because it's less efficient.
    fn render_frames(&self, icon_state: &State) -> Vec<Image> {
        let frames = match &icon_state.frames {
            Frames::One => 1,
            Frames::Count(count) => *count,
            Frames::Delays(delays) => delays.len(),
        };
        let mut canvas = self.get_canvas(icon_state.dirs);
        let mut vec = Vec::new();
        let range = if icon_state.rewind {
            Either::Left((0..frames).chain((0..frames).rev()))
        } else {
            Either::Right(0..frames)
        };
        for frame in range {
            self.render_dirs(icon_state, &mut canvas, frame as u32);
            vec.push(canvas.clone());
            canvas.clear();
        }
        vec
    }

    /// Returns a new canvas of the appropriate size
    fn get_canvas(&self, dirs: Dirs) -> Image {
        match dirs {
            Dirs::One => Image::new_rgba(self.source.metadata.width, self.source.metadata.height),
            Dirs::Four => {
                Image::new_rgba(self.source.metadata.width * 4, self.source.metadata.height)
            }
            Dirs::Eight => {
                Image::new_rgba(self.source.metadata.width * 8, self.source.metadata.height)
            }
        }
    }

    /// Gives a [`Vec<Dir>`] of each [`Dir`] matching our [`Dirs`] setting,
    /// in the same order BYOND uses.
    fn ordered_dirs(dirs: Dirs) -> Vec<Dir> {
        match dirs {
            Dirs::One => [Dir::South].to_vec(),
            Dirs::Four => [Dir::South, Dir::North, Dir::East, Dir::West].to_vec(),
            Dirs::Eight => [
                Dir::South,
                Dir::North,
                Dir::East,
                Dir::West,
                Dir::Southeast,
                Dir::Southwest,
                Dir::Northeast,
                Dir::Northwest,
            ]
            .to_vec(),
        }
    }

    /// Renders each direction to the same canvas, offsetting them to the right
    fn render_dirs(&self, icon_state: &State, canvas: &mut Image, frame: u32) {
        for (dir_no, dir) in Self::ordered_dirs(icon_state.dirs).iter().enumerate() {
            let frame_idx = icon_state.index_of_frame(*dir, frame as u32);
            let frame_rect = self.source.rect_of_index(frame_idx);
            canvas.composite(
                &self.source.image,
                (self.source.metadata.width * (dir_no as u32), 0),
                frame_rect,
                NO_TINT,
            );
        }
    }

    /// Renders the whole file to a gif, animated states becoming frames
    fn render_gif<W: std::io::Write>(&self, icon_state: &State, target: W) -> io::Result<()> {
        if !icon_state.is_animated() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Tried to render gif with one frame",
            ));
        }

        let (frames, delays) = match &icon_state.frames {
            Frames::Count(frames) => (*frames, None),
            Frames::Delays(delays) => (delays.len(), Some(delays)),
            _ => unreachable!(),
        };

        let mut canvas = self.get_canvas(icon_state.dirs);

        let mut encoder = gif::Encoder::new(target, canvas.width as u16, canvas.height as u16, &[])
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{e}")))?;

        encoder
            .set_repeat(gif::Repeat::Infinite)
            .map_err(|e| io::Error::new(io::ErrorKind::Other, format!("{e}")))?;

        let range = if icon_state.rewind {
            Either::Left((0..frames).chain((0..frames).rev()))
        } else {
            Either::Right(0..frames)
        };

        for frame in range {
            self.render_dirs(icon_state, &mut canvas, frame as u32);

            let mut pixels = bytemuck::cast_slice(canvas.data.as_slice().unwrap()).to_owned();
            let mut gif_frame =
                gif::Frame::from_rgba(canvas.width as u16, canvas.height as u16, &mut pixels);
            // gif::Frame delays are measured in "Frame delay in units of 10 ms."
            // aka centiseconds. We're measuring in BYOND ticks, aka deciseconds.
            // And it's a u16 for some reason so we just SHRUG and floor it.
            gif_frame.delay =
                (delays.map_or_else(|| 1.0, |f| *f.get(frame).unwrap_or(&1.0)) * 10.0) as u16;
            // the disposal method by default is "keep the previous frame under the alpha mask"
            // wtf
            gif_frame.dispose = DisposalMethod::Background;
            encoder.write_frame(&gif_frame).unwrap();
            // Clear the canvas.
            canvas.clear();
        }

        Ok(())
    }

    /// Renders the whole file to a png, discarding all but the first frame of animations
    fn render_to_png<W: std::io::Write>(&self, icon_state: &State, target: W) -> io::Result<()> {
        let mut canvas = self.get_canvas(icon_state.dirs);

        self.render_dirs(icon_state, &mut canvas, 0);

        canvas.to_write(target)?;
        // Always remember to clear the canvas for the next guy!
        canvas.clear();
        Ok(())
    }
}
