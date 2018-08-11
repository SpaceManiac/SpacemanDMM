//! The built-in template.

use tera::Tera;

pub fn builtin() -> Result<Tera, ::tera::Error> {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("macros.html", include_str!("macros.html")),
        ("base.html", include_str!("base.html")),
        ("dm_index.html", include_str!("dm_index.html")),
        ("dm_type.html", include_str!("dm_type.html")),
    ])?;
    Ok(tera)
}

pub const RESOURCES: &[(&str, &str)] = &[
    ("dmdoc.css", include_str!("dmdoc.css")),
    ("dmdoc.js", include_str!("dmdoc.js")),
];
