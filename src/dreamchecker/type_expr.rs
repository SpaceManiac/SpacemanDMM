//! Support for "type expressions", used in evaluating dynamic/generic return
//! types.

use dm::{Location, DMError};
use dm::ast::*;

use StaticType;

pub enum TypeExpr<'o> {
    Static(StaticType<'o>),
}

impl<'o> TypeExpr<'o> {
    pub fn compile(location: Location, expression: &Expression) -> Result<TypeExpr<'o>, DMError> {
        Err(DMError::new(location, "not yet implemented"))
    }

    pub fn evaluate(&self) -> StaticType<'o> {
        match self {
            TypeExpr::Static(st) => st.clone(),
        }
    }
}

impl<'o> From<StaticType<'o>> for TypeExpr<'o> {
    fn from(static_type: StaticType<'o>) -> TypeExpr<'o> {
        TypeExpr::Static(static_type)
    }
}
