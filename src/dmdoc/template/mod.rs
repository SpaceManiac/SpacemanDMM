//! The built-in template.

use tera::{Tera, Value};

pub fn builtin() -> Result<Tera, ::tera::Error> {
    let mut tera = Tera::default();
    tera.add_raw_templates(vec![
        ("macros.html", include_str!("macros.html")),
        ("dm_index.html", include_str!("dm_index.html")),
        ("dm_type.html", include_str!("dm_type.html")),
    ])?;
    tera.register_filter("md", |input, opts| match input {
        Value::String(s) => Ok(Value::String(::render_markdown(&s, opts.contains_key("teaser")))),
        other => Ok(other),
    });
    Ok(tera)
}
