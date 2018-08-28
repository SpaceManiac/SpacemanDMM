//! Placement and editing tools which appear in the workbench.

use dm::objtree::ObjectTree;

pub struct Tool {
    pub name: &'static str,
    pub objtree: bool,
    pub behavior: Box<ToolBehavior>,
}

pub trait ToolBehavior {
}

impl Tool {
    fn new<B: ToolBehavior + 'static>(name: &'static str, behavior: B) -> Tool {
        Tool {
            name,
            objtree: false,
            behavior: Box::new(behavior),
        }
    }

    fn show_objtree(self) -> Self {
        Tool { objtree: true, ..self }
    }
}

pub fn configure(_objtree: &ObjectTree) -> Vec<Tool> {
    let mut tools = Vec::new();
    tools.push(Tool::new("Place", Place).show_objtree());
    tools.push(Tool::new("Rectangle", Rectangle).show_objtree());
    tools.push(Tool::new("Select", Select).show_objtree());
    tools
}

// ----------------------------------------------------------------------------
// Basic tools that are impossible to do without

struct Place;
impl ToolBehavior for Place {
}

struct Rectangle;
impl ToolBehavior for Rectangle {
}

struct Select;
impl ToolBehavior for Select {
}
