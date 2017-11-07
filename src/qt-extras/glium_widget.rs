//! A Glium backend which uses Qt's OpenGL facilites.
#![allow(improper_ctypes)]

use std::rc::Rc;
use std::os::raw::c_void;

use libc::c_int;

use cpp_utils::{CppBox, CppDeletable, Deleter, StaticCast};
use widgets::opengl_widget::OpenGLWidget;
use widgets::qt_core::byte_array::ByteArray;

use glium;
use glium::backend::{Context, Backend, Facade};
use glium::SwapBuffersError;

// ----------------------------------------------------------------------------
// The Rust interface

/// Trait defining the behavior of a Glium widget.
#[allow(unused_variables)]
pub trait GliumCallbacks {
    /// Data constructed by `initialized()`, usually GL handles.
    type Data;

    /// Initialize this widget's associated OpenGL data.
    ///
    /// Always called before the first `resize()` or `paint()`.
    fn initialize(&mut self, display: &Display) -> Self::Data;

    /// Called when the widget has resized, to allow updating the viewport.
    ///
    /// Always called before the first `paint()`.
    fn resize(&mut self, display: &Display, data: &mut Self::Data, w: u32, h: u32) {}

    /// Called by Qt when the widget needs to be repainted.
    fn paint(&mut self, display: &Display, data: &mut Self::Data) {}
}

/// Construct an OpenGLWidget backed by the provided callbacks.
pub fn create<C: GliumCallbacks + 'static>(cb: C) -> CppBox<OpenGLWidget> {
    unsafe {
        let boxed: Box<Box<ForeignHooks>> = Box::new(Box::new(DisplayHolder::new(cb)));
        CppBox::new(::cpp_utils::static_cast_mut(GliumWidget_new(Box::into_raw(boxed))))
    }
}

/// A subtype of `OpenGLWidget` which calls Rust callbacks for GL functions.
#[repr(C)]
#[doc(hidden)]
pub struct GliumWidget(u8);

impl<T> StaticCast<T> for GliumWidget where OpenGLWidget: StaticCast<T> {
    fn static_cast(&self) -> &T {
        unsafe { (*(self as *const GliumWidget as *const OpenGLWidget)).static_cast() }
    }
    fn static_cast_mut(&mut self) -> &mut T {
        unsafe { (*(self as *mut GliumWidget as *mut OpenGLWidget)).static_cast_mut() }
    }
}

/// A handle to a ready-to-render `OpenGLWidget`.
pub struct Display {
    size: (u32, u32),
    ctx: Rc<Context>,
}

impl Display {
    /// Begin drawing to the widget's surface.
    pub fn draw(&self) -> glium::Frame {
        glium::Frame::new(self.ctx.clone(), self.size)
    }
}

impl Facade for Display {
    fn get_context(&self) -> &Rc<Context> {
        &self.ctx
    }
}

/// An internal holder for a callback object and associated data.
struct DisplayHolder<C: GliumCallbacks> {
    cb: C,
    data: Option<(Display, C::Data)>,
}

impl<C: GliumCallbacks> DisplayHolder<C> {
    unsafe fn new(cb: C) -> Self {
        DisplayHolder {
            cb,
            data: None,
        }
    }
}

unsafe impl<C: GliumCallbacks> ForeignHooks for DisplayHolder<C> {
    unsafe fn initialize(&mut self, p: &mut GliumWidget) {
        // Context is current, but there is not yet a framebuffer
        let backend = QTBackend(::cpp_utils::static_cast_mut(p));
        let ctx = Context::new(backend, false, Default::default()).unwrap();
        let display = Display {
            size: (0, 0),
            ctx: ctx,
        };
        let data = self.cb.initialize(&display);
        self.data = Some((display, data));
    }

    unsafe fn resize(&mut self, _: &mut GliumWidget, w: u32, h: u32) {
        // Context is current and the framebuffer is bound
        let &mut (ref mut display, ref mut data) = self.data.as_mut().unwrap();
        display.size = (w, h);
        self.cb.resize(display, data, w, h);
    }

    unsafe fn paint(&mut self, _: &mut GliumWidget) {
        // Context is current and the framebuffer is bound
        let &mut (ref mut display, ref mut data) = self.data.as_mut().unwrap();
        self.cb.paint(display, data);
    }
}

// ----------------------------------------------------------------------------
// The C++ integration

#[doc(hidden)]
pub unsafe trait ForeignHooks {
    unsafe fn initialize(&mut self, p: &mut GliumWidget);
    unsafe fn resize(&mut self, p: &mut GliumWidget, w: u32, h: u32);
    unsafe fn paint(&mut self, p: &mut GliumWidget);
}

cpp! {{
    #include <QtWidgets/QOpenGLWidget.h>

    class GliumWidget;
    extern "C" void GliumWidget_initializeGL(GliumWidget*, void*);
    extern "C" void GliumWidget_resizeGL(GliumWidget*, void*, int, int);
    extern "C" void GliumWidget_paintGL(GliumWidget*, void*);
    extern "C" void GliumWidget_drop(void*);

    class GliumWidget : public QOpenGLWidget {
    protected:
        void* data;
        void initializeGL() {
            GliumWidget_initializeGL(this, data);
        }
        void resizeGL(int w, int h) {
            GliumWidget_resizeGL(this, data, w, h);
        }
        void paintGL() {
            GliumWidget_paintGL(this, data);
        }
    public:
        GliumWidget(void* data) : QOpenGLWidget(), data(data) {}
        ~GliumWidget() { GliumWidget_drop(data); }
    };

    extern "C" GliumWidget* GliumWidget_new(void* data) {
        return new GliumWidget(data);
    }
    extern "C" void GliumWidget_delete(GliumWidget* p) {
        delete p;
    }
}}

extern {
    fn GliumWidget_new(data: *mut Box<ForeignHooks>) -> *mut GliumWidget;
    fn GliumWidget_delete(p: *mut GliumWidget);
}

#[no_mangle]
#[doc(hidden)]
pub unsafe extern fn GliumWidget_initializeGL(p: &mut GliumWidget, dh: &mut Box<ForeignHooks>) {
    dh.initialize(p);
}

#[no_mangle]
#[doc(hidden)]
pub unsafe extern fn GliumWidget_resizeGL(p: &mut GliumWidget, dh: &mut Box<ForeignHooks>, w: c_int, h: c_int) {
    dh.resize(p, w as u32, h as u32);
}

#[no_mangle]
#[doc(hidden)]
pub unsafe extern fn GliumWidget_paintGL(p: &mut GliumWidget, dh: &mut Box<ForeignHooks>) {
    dh.paint(p);
}

#[no_mangle]
#[doc(hidden)]
pub unsafe extern fn GliumWidget_drop(_: Box<Box<ForeignHooks>>) {}

impl CppDeletable for GliumWidget {
    fn deleter() -> Deleter<Self> {
        GliumWidget_delete
    }
}

// ----------------------------------------------------------------------------
// Glium backend implementation

struct QTBackend(*mut OpenGLWidget);

unsafe impl Backend for QTBackend {
    fn swap_buffers(&self) -> Result<(), SwapBuffersError> {
        Ok(())  // QT will do this for us
    }

    unsafe fn get_proc_address(&self, symbol: &str) -> *const c_void {
        // Despite accepting a QByteArray, which has a length specified,
        // get_proc_address needs it to be null-terminated! What's even the
        // point of that? We are thus forced to heap-allocate...
        let mut bytes = ByteArray::new(());
        for ch in symbol.bytes() {
            bytes.push_back(ch as i8);
        }
        bytes.push_back(0);

        (*(*self.0).context()).get_proc_address(&bytes) as *const c_void
    }

    fn get_framebuffer_dimensions(&self) -> (u32, u32) {
        // Best-effort estimate
        let size = unsafe { (*self.0).size() };
        (size.width() as u32, size.height() as u32)
    }

    fn is_current(&self) -> bool {
        true  // hopefully
    }

    unsafe fn make_current(&self) {
        // do nothing
    }
}
