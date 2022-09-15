//! Color handling.
//!
//! Uses a regular expression on the source text to handle `#define`s and
//! colors inside HTML blocks, which an annotation for strings or `rgb()` calls
//! would not catch.

use regex::Regex;

/// Extract ranges and colors from an input string.
pub fn extract_colors(input: &str) -> impl Iterator<Item = (usize, usize, [u8; 4])> + '_ {
    COLOR_REGEX.captures_iter(input).flat_map(|capture| {
        parse_capture(&capture).map(|rgba| {
            let totality = capture.get(0).unwrap();
            (totality.start(), totality.end(), rgba)
        })
    })
}

#[derive(Copy, Clone)]
pub enum ColorFormat {
    Hex {
        // TODO: uppercase
        single_quoted: bool,
        short: bool,
        alpha: bool,
    },
    Rgb {
        alpha: bool,
    },
}

impl ColorFormat {
    pub fn parse(input: &str) -> Option<ColorFormat> {
        if input.starts_with("rgb") {
            return Some(ColorFormat::Rgb {
                alpha: input.chars().filter(|&c| c == ',').count() > 2,
            });
        };
        let single_quoted = if input.starts_with("'#") && input.ends_with('\'') {
            true
        } else if input.starts_with("\"#") && input.ends_with('"') {
            false
        } else {
            return None;
        };
        Some(ColorFormat::Hex {
            single_quoted,
            short: input.len() <= 7,
            // "#rgba" or "#rrggbbaa"
            alpha: input.len() == 7 || input.len() == 11,
        })
    }

    pub fn format(self, [r, g, b, a]: [u8; 4]) -> String {
        match self {
            ColorFormat::Hex {
                single_quoted,
                short,
                alpha,
            } => {
                let q = if single_quoted { '\'' } else { '"' };
                let short =
                    short && r % 0x11 == 0 && g % 0x11 == 0 && b % 0x11 == 0 && a % 0x11 == 0;
                let alpha = alpha || a != 255;
                match (short, alpha) {
                    (false, false) => format!("{}#{:02x}{:02x}{:02x}{}", q, r, g, b, q),
                    (false, true) => format!("{}#{:02x}{:02x}{:02x}{:02x}{}", q, r, g, b, a, q),
                    (true, false) => {
                        format!("{}#{:x}{:x}{:x}{}", q, r / 0x11, g / 0x11, b / 0x11, q)
                    }
                    (true, true) => format!(
                        "{}#{:x}{:x}{:x}{:x}{}",
                        q,
                        r / 0x11,
                        g / 0x11,
                        b / 0x11,
                        a / 0x11,
                        q
                    ),
                }
            }
            ColorFormat::Rgb { alpha } if alpha || a != 255 => {
                format!("rgb({}, {}, {}, {})", r, g, b, a)
            }
            ColorFormat::Rgb { alpha: _ } => format!("rgb({}, {}, {})", r, g, b),
        }
    }
}

impl Default for ColorFormat {
    fn default() -> ColorFormat {
        ColorFormat::Hex {
            single_quoted: false,
            short: false,
            alpha: false,
        }
    }
}

lazy_static! {
    // 3-8 digit hex colors within "#..." or '#...' and rgb() calls
    static ref COLOR_REGEX: Regex = Regex::new(r##""#([0-9A-Fa-f]{3,8})"|'#([0-9A-Fa-f]{3,8})'|rgb\(\s*(\d{1,3}),\s*(\d{1,3}),\s*(\d{1,3})(?:,\s*(\d{1,3}))?\s*\)"##).unwrap();
}

fn parse_capture(capture: &regex::Captures) -> Option<[u8; 4]> {
    // Tied closely to the regex above.
    match (
        capture.get(1),
        capture.get(2),
        capture.get(3),
        capture.get(4),
        capture.get(5),
        capture.get(6),
    ) {
        (Some(cap), _, _, _, _, _) | (_, Some(cap), _, _, _, _) => parse_hex(cap.as_str()),
        (_, _, Some(r), Some(g), Some(b), a) => {
            parse_rgba(r.as_str(), g.as_str(), b.as_str(), a.map(|a| a.as_str()))
        }
        _ => None,
    }
}

fn parse_hex(hex: &str) -> Option<[u8; 4]> {
    let mut sum = 0;
    for ch in hex.chars() {
        sum = 16 * sum + ch.to_digit(16).unwrap_or(0);
    }

    if hex.len() == 8 {
        // #rrggbbaa
        Some([
            (sum >> 24) as u8,
            (sum >> 16) as u8,
            (sum >> 8) as u8,
            sum as u8,
        ])
    } else if hex.len() == 6 {
        // #rrggbb
        Some([(sum >> 16) as u8, (sum >> 8) as u8, sum as u8, 255])
    } else if hex.len() == 4 {
        // #rgba
        Some([
            (0x11 * ((sum >> 12) & 0xf)) as u8,
            (0x11 * ((sum >> 8) & 0xf)) as u8,
            (0x11 * ((sum >> 4) & 0xf)) as u8,
            (0x11 * (sum & 0xf)) as u8,
        ])
    } else if hex.len() == 3 {
        // #rgb
        Some([
            (0x11 * ((sum >> 8) & 0xf)) as u8,
            (0x11 * ((sum >> 4) & 0xf)) as u8,
            (0x11 * (sum & 0xf)) as u8,
            255,
        ])
    } else {
        None
    }
}

fn parse_rgba(r: &str, g: &str, b: &str, a: Option<&str>) -> Option<[u8; 4]> {
    Some([
        r.parse::<u8>().ok()?,
        g.parse::<u8>().ok()?,
        b.parse::<u8>().ok()?,
        a.and_then(|a| a.parse::<u8>().ok()).unwrap_or(255),
    ])
}
