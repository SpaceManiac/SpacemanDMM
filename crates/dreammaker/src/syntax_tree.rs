use crate::{objtree::{ObjectTree, TypeVar, TypeProc, VarDeclaration, VarValue, SymbolIdSource, SymbolIdCategory, ProcValue, ProcDeclaration, ObjectTreeBuilder}, Location, docs::DocCollection, lexer::{LocatedToken, Token}, ast::{Expression, VarType, Parameter, ProcDeclKind, Block, Ident}, Context, annotation::{AnnotationTree, Annotation}, DMError, Severity};

use std::collections::{HashSet, VecDeque};

pub enum TreeEntryData {
    Decl,
    Block(TreeBlock),
    Proc(TypeProc, String, Location),
    Var(TypeVar, String),
}

pub struct TreeEntry {
    leading_path: Vec<String>,
    start: Location,
    end: Location,
    absolute: bool,
    data: TreeEntryData,
    docs: DocCollection,

    // Macro usage that affects the integrity of this TreeEntry
    macro_pollution: HashSet<String>,
}

impl TreeEntry {
    fn new(mut leading_path: Vec<String>, start: Location, absolute: bool) -> Self {
        leading_path.shrink_to_fit();
        TreeEntry {
            leading_path,
            start,
            end: start,
            absolute,
            data: TreeEntryData::Decl,
            docs: DocCollection::default(),
            macro_pollution: HashSet::default()
        }
    }
}

pub struct TreeEntryBuilder<'entry> {
    parent_path: &'entry Vec<String>,
    tree_entry: &'entry mut TreeEntry,
}

impl<'entry> TreeEntryBuilder<'entry> {
    pub fn get_path(&self) -> Vec<String> {
        let mut path = self.parent_path.clone();
        path.extend_from_slice(&self.tree_entry.leading_path);
        path
    }

    pub fn finish(mut self, end: Location, macro_pollution: HashSet<String>) {
        self.tree_entry.end = end;
        self.tree_entry.macro_pollution = macro_pollution;
    }

    pub fn extend_docs(&mut self, docs: DocCollection) {
        self.tree_entry.docs.extend(docs)
    }

    pub fn as_block<'s>(&'s mut self) -> TreeBlockBuilder<'s> {
        assert!(matches!(self.tree_entry.data, TreeEntryData::Decl));
        self.tree_entry.data = TreeEntryData::Block(TreeBlock::default());

        let parent_path = self.get_path();
        if let TreeEntryData::Block(block) = &mut self.tree_entry.data {
            return TreeBlockBuilder {
                parent_path,
                tree_block: block
            }
        }

        panic!("Literally how")
    }

    pub fn as_var(&mut self, name: String, location: Location, docs: DocCollection, var_type_option: Option<VarType>, expression: Option<Expression>) {
        assert!(matches!(self.tree_entry.data, TreeEntryData::Decl));

        let declaration: Option<VarDeclaration> = match var_type_option {
            Some(var_type) => {
                // we don't actually use symbols in the SyntaxTree
                let mut symbols = SymbolIdSource::new(SymbolIdCategory::ObjectTree);
                Some(VarDeclaration {
                    var_type,
                    location,
                    id: symbols.allocate(),
                })
            },
            None => None,
        };
        let var = TypeVar {
            value: VarValue {
                location,
                expression,
                docs,
                constant: None,
                being_evaluated: false,
            },
            declaration,
        };

        self.tree_entry.data = TreeEntryData::Var(var, name);
    }

    pub fn as_proc(
        &mut self,
        name: String,
        location: Location,
        kind_option: Option<ProcDeclKind>,
        parameters: Vec<Parameter>,
        code: Option<Block>,
        docs: DocCollection,
        body_start: Location,
    ) {
        assert!(matches!(self.tree_entry.data, TreeEntryData::Decl));

        let declaration = if let Some(kind) = kind_option {
            // we don't actually use symbols in the SyntaxTree
            let mut symbols = SymbolIdSource::new(SymbolIdCategory::ObjectTree);
            Some(ProcDeclaration {
                location,
                kind,
                id: symbols.allocate(),
                is_private: false,
                is_protected: false,
            })
        } else {
            None
        };

        let proc: TypeProc = TypeProc {
            value: vec![ProcValue {
                location,
                parameters: parameters.into(),
                docs,
                code
            }],
            declaration,
        };

        self.tree_entry.data = TreeEntryData::Proc(proc, name, body_start);
    }
}

#[derive(Default)]
pub struct TreeBlock {
    entries: Vec<TreeEntry>,
}

pub struct TreeBlockBuilder<'entry> {
    parent_path: Vec<String>,
    tree_block: &'entry mut TreeBlock,
}

impl<'entry> TreeBlockBuilder<'entry> {
    pub fn is_root(&self) -> bool {
        self.parent_path.len() == 0
    }

    pub fn entry<'s>(&'s mut self, relative_path: Vec<String>, location: Location, absolute: bool) -> TreeEntryBuilder<'s> {
        self.tree_block.entries.push(TreeEntry::new(relative_path, location, absolute));
        TreeEntryBuilder {
            parent_path: &self.parent_path,
            tree_entry: self.tree_block.entries.last_mut().unwrap(),
        }
    }
}

pub struct SyntaxTree<'ctx> {
    context: &'ctx Context,
    tokens: Vec<Token>,
    root: TreeBlock,
    parser_fatal_errored: bool,
}

impl<'ctx> SyntaxTree<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        SyntaxTree {
            context,
            tokens: Default::default(),
            root: Default::default(),
            parser_fatal_errored: false,
        }
    }

    pub fn object_tree_with_annotations(&self, annotations: &mut AnnotationTree) -> ObjectTree {
        self.generate_objtree(Some(annotations), true)
    }

    pub fn object_tree(&mut self) -> ObjectTree {
        self.generate_objtree(None, true)
    }

    pub fn root(&self) -> &TreeBlock {
        &self.root
    }

    pub(crate) fn build_root(&mut self) -> TreeBlockBuilder {
        TreeBlockBuilder {
            parent_path: Vec::default(),
            tree_block: &mut self.root
        }
    }

    pub(crate) fn finish(&mut self, parser_fatal_errored: bool) {
        self.root.entries.shrink_to_fit();
        self.parser_fatal_errored = parser_fatal_errored;
    }

    pub fn add_token(&mut self, token: LocatedToken) {
        self.tokens.push(token.token)
    }

    pub fn object_tree_without_builtins(&mut self) -> ObjectTree {
        self.generate_objtree(None, false)
    }

    fn generate_objtree(&self, annotations: Option<&mut AnnotationTree>, builtins: bool) -> ObjectTree {
        let mut builder = ObjectTreeBuilder::default();
        if builtins {
            builder.register_builtins();
        }

        self.recurse_tree(&mut builder, annotations);

        builder.finish(self.context, self.parser_fatal_errored)
    }

    fn recurse_tree(&self, builder: &mut ObjectTreeBuilder, mut annotations: Option<&mut AnnotationTree>) {
        let mut stack = VecDeque::with_capacity(1);
        stack.push_back((&self.root, builder.root_index(), 0));

        while let Some((current_block, block_parent, parent_depth)) = stack.pop_back() {
            for entry in &current_block.entries {
                let mut current = block_parent;
                let mut current_depth = parent_depth;

                for path_segment in &entry.leading_path {
                    current_depth = current_depth + 1;
                    current = builder.subtype_or_add(entry.start, current, path_segment, current_depth);
                }

                match &entry.data {
                    TreeEntryData::Decl => {
                        // subtype_or_add calls above were all that was necessary
                    },
                    TreeEntryData::Block(block) => stack.push_back((block, current, current_depth)),
                    TreeEntryData::Proc(proc, name, body_start) => {
                        let decl_kind = if let Some(declaration) = &proc.declaration {
                            Some(declaration.kind)
                        } else {
                            None
                        };

                        let proc_value = proc.main_value();
                        let result = builder.register_proc(
                            self.context,
                            proc.main_value().location,
                            current,
                            name,
                            decl_kind,
                            proc_value.parameters.clone(),
                            proc_value.code.clone());

                        match result {
                            Ok((idx, new_proc)) => {
                                new_proc.docs.extend(proc_value.docs.clone());
                                // manually performed for borrowck reasons
                                if let Some(dest) = &mut annotations {
                                    let new_stack = reconstruct_path(builder.get_path(current), decl_kind, &name);
                                    dest.insert(entry.start..*body_start, Annotation::ProcHeader(new_stack.to_vec(), idx));
                                    dest.insert(*body_start..entry.end, Annotation::ProcBody(new_stack.to_vec(), idx));
                                }
                                if !entry.absolute && self.context.config().code_standards.disallow_relative_proc_definitions {
                                    DMError::new(entry.start, "relatively pathed proc defined here")
                                        .set_severity(Severity::Warning)
                                        .register(self.context);
                                }
                            }
                            Err(e) => self.context.register_error(e),
                        };
                    },
                    TreeEntryData::Var(var, name) => {
                        if let Some(declaration) = &var.declaration {
                            builder.declare_var(
                                current,
                                name,
                                var.value.location,
                                var.value.docs.clone(),
                                declaration.var_type.clone(),
                                var.value.expression.clone());
                        } else {
                            builder.override_var(
                                current,
                                name,
                                var.value.location,
                                var.value.docs.clone(),
                                var.value.expression.as_ref().unwrap().clone());
                        }
                    },
                }
            }
        }
    }
}

fn reconstruct_path(node: &str, proc_kind: Option<ProcDeclKind>, last: &str) -> Vec<Ident> {
    let mut result = Vec::new();
    for entry in node.split('/').skip(1) {
        result.push(entry.to_owned());
    }
    if let Some(kind) = proc_kind {
        result.push(kind.to_string());
    }

    if !last.is_empty() {
        result.push(last.to_owned());
    }
    result
}

// random notes
// updating any file requires a full reparse of the remainder of the file in order to update locations/symbolids
// tree entries spanning files will always need reparsing if affected?
// macro updates don't require a full reparse of affected files, just the polluted tree entry
