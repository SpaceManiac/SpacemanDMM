//! The built-in template.

use tera::Tera;

#[cfg(debug_assertions)]
pub fn builtin() -> Result<Tera, ::tera::Error> {
    Tera::new(concat!(env!("CARGO_MANIFEST_DIR"), "/template/*.html"))
}

#[cfg(not(debug_assertions))]
pub fn builtin() -> Result<Tera, ::tera::Error> {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("macros.html", include_str!("macros.html")),
        ("base.html", include_str!("base.html")),
        ("dm_index.html", include_str!("dm_index.html")),
        ("dm_type.html", include_str!("dm_type.html")),
        ("dm_module.html", include_str!("dm_module.html")),
    ])?;
    Ok(tera)
}

pub const RESOURCES: &[(&str, &[u8])] = &[
    ("dmdoc.css", include_bytes!("dmdoc.css")),
    ("dmdoc.js", include_bytes!("dmdoc.js")),
    ("git.png", include_bytes!("git.png")),
];
