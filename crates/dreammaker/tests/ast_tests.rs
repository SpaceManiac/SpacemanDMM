extern crate dreammaker as dm;

use core::panic;

use dm::*;
use dm::constants::*;
use dm::ast::*;
use dm::preprocessor::Preprocessor;
use dm::objtree::ObjectTree;

fn with_code<F: FnOnce(Context, ObjectTree)>(code: &'static str, f: F) {
    let context = Context::default();
    let path = std::path::PathBuf::from(r"test.dm");
    let pp = Preprocessor::from_buffer(&context, path, code.trim());
    let indents = indents::IndentProcessor::new(&context, pp);
    let mut parser = parser::Parser::new(&context, indents);
    parser.enable_procs();
    let _tree = parser.parse_object_tree();

    f(context, _tree)
}


#[test]
fn check_semicolon_in_proc_parameters() {
    with_code("
#define DEF1 0x01;
#define DEF2 \"asdf\" as text;

/proc/darn(foo = DEF1, bar = DEF2, anotherarg = 1)
", |context, _| {
        let errors = context.errors();
        assert_eq!(errors.len(), 2);

        for error in errors.iter() {
            assert_eq!(error.errortype().expect("No errortype set!"), "semicolon_in_proc_parameter");
        }
    });
}

#[test]
fn process_scope() {
    with_code("
/datum/test
    var/hell = type::base
    var/base = 10
    var/heck = type::base
    var/static/stat = 1
    var/proc_holder = type::reference()

/datum/test/proc/reference()

/datum/test/sub
    base = parent_type::base + 10
    heck = /datum/test::heck + 2

var/global/bill = 1

/proc/work()
    var/x = /datum/test::base
    var/datum/test/larry = /datum/test
    x = larry::stat
    x = /datum/test::reference()
    /datum/test::stat = 2
    ::extra()
    x = ::bill

/proc/extra()
", |context, tree| {
        let errors = context.errors();

        // Check for errors
        let mut sum_errors: Vec<String> = vec![];
        for error in errors.iter() {
            sum_errors.push(format!("{}", error));
        }
        if !sum_errors.is_empty() {
            panic!("\n{}", sum_errors.join("\n").as_str());
        }
        // test type::var in typedef
        let parent_type = tree.find("/datum/test").unwrap();
        let type_read = parent_type.get_value("heck").unwrap();
        let Some(constant) = type_read.constant.as_ref() else {
            panic!("Failed to constant evaluate :: operator")
        };
        if let Constant::Float(value) = constant {
            assert_eq!(*value, 10f32)
        } else {
            panic!("{} was expected to be a float, but it wasn't!", type_read.constant.as_ref().unwrap())
        }
        // test type::proc() in typedef
        let parent_type = tree.find("/datum/test").unwrap();
        let type_read = parent_type.get_value("proc_holder").unwrap();
        let Some(constant) = type_read.constant.as_ref() else {
            panic!("Failed to constant evaluate :: proc operator")
        };
        if let Constant::Prefab(value) = constant {
            let pop_list = FormatTreePath(&value.path).to_string();
            assert_eq!(pop_list, "/datum/test/proc/reference")
        } else {
            panic!("{} was expected to be a path, but it wasn't!", type_read.constant.as_ref().unwrap())
        }
        //  parent_type::var in a subtype
        let child_type = tree.find("/datum/test/sub").unwrap();
        let type_read = child_type.get_value("base").unwrap();
        let Some(constant) = type_read.constant.as_ref() else {
            panic!("Failed to constant evaluate :: operator")
        };
        if let Constant::Float(value) = constant {
            assert_eq!(*value, 10f32 + 10f32)
        } else {
            panic!("{} was expected to be a float, but it wasn't!", type_read.constant.as_ref().unwrap())
        }
        // /datum/explicit::var in a type
        let child_type = tree.find("/datum/test/sub").unwrap();
        let type_read = child_type.get_value("heck").unwrap();
        let Some(constant) = type_read.constant.as_ref() else {
            panic!("Failed to constant evaluate :: operator")
        };
        if let Constant::Float(value) = constant {
            assert_eq!(*value, 10f32 + 2f32)
        } else {
            panic!("{} was expected to be a float, but it wasn't!", type_read.constant.as_ref().unwrap())
        }
        let global_procs = tree.root();
        let work_proc = global_procs.get_proc("work").unwrap();
        let work_code = work_proc.code.as_ref().unwrap().iter().map(|statement| {
            &statement.elem
        }).collect::<Vec<&Statement>>();
        // /datum/explicit::var
        let Statement::Var(x_init) = work_code.first().unwrap() else {
            panic!("First statement was not an expression")
        };
        let Expression::Base { term: _, follow } = x_init.value.as_ref().unwrap() else {
            panic!("/datum/test::base was NOT evaluated as a base expression")
        };
        match &follow.first().unwrap().elem {
            Follow::StaticField(field) => {
                if field != "base" {
                    panic!("/datum/test::base did not eval base as the var to read")
                }
            }
            _ => panic!("/datum/test::base failed to eval :: as a static field")
        }
        // implicit_type::variable
        let Statement::Expr(larrys_read) = work_code.get(2).unwrap() else {
            panic!("Third statement was not an expression")
        };

        let Expression::AssignOp { op: _, lhs: _, rhs } = larrys_read else {
            panic!("x = larry::stat was NOT evaluated as an assignment expression")
        };
        let Expression::Base { term: _, ref follow } = **rhs else {
            panic!("larry::stat was NOT evaluated as a base expression")
        };
        match &follow.first().unwrap().elem {
            Follow::StaticField(field) => {
                if field != "stat" {
                    panic!("larry::stat did not eval stat as the var to read")
                }
            }
            _ => panic!("larry::stat failed to eval :: as a static field")
        }

        // /datum/explicit::proc()
        let Statement::Expr(proc_ref) = work_code.get(3).unwrap() else {
            panic!("Fourth statement was not an expression")
        };

        let Expression::AssignOp { op: _, lhs: _, rhs } = proc_ref else {
            panic!("x = /datum/test::reference() was NOT evaluated as an assignment expression")
        };
        let Expression::Base { term: _, ref follow } = **rhs else {
            panic!("/datum/test::reference() was NOT evaluated as a base expression")
        };
        match &follow.first().unwrap().elem {
            Follow::ProcReference(proc_name) => {
                if proc_name != "reference" {
                    panic!("/datum/test::reference() did not eval reference() as the ref to read")
                }
            }
            _ => panic!("/datum/test::reference() failed to eval :: as a proc reference")
        }
        // /datum/explicit::static_var = value
        let Statement::Expr(static_set) = work_code.get(4).unwrap() else {
            panic!("Fifth statement was not an expression")
        };

        let Expression::AssignOp { op: _, lhs, rhs: _ } = static_set else {
            panic!("/datum/test::stat = 2 was NOT evaluated as an assignment expression")
        };
        let Expression::Base { term: _, ref follow } = **lhs else {
            panic!("/datum/test::stat was NOT evaluated as a base expression")
        };
        match &follow.first().unwrap().elem {
            Follow::StaticField(field) => {
                if field != "stat" {
                    panic!("/datum/test::stat did not eval stat as the var to set")
                }
            }
            _ => panic!("/datum/test::stat failed to eval :: as a static field")
        }
        // ::global_proc()
        let Statement::Expr(static_set) = work_code.get(5).unwrap() else {
            panic!("Sixth statement was not an expression")
        };

        let Expression::Base { term, follow: _ } = static_set else {
            panic!("::extra() was NOT evaluated as a base expression")
        };
        match &term.elem {
            Term::GlobalCall(function, _) => {
                if function != "extra" {
                    panic!("::extra() did not eval extra as the proc to call")
                }
            }
            _ => panic!("::extra() failed to eval :: as a global call")
        }
        // ::global_var
        let Statement::Expr(proc_ref) = work_code.get(6).unwrap() else {
            panic!("Seventh statement was not an expression")
        };

        let Expression::AssignOp { op: _, lhs: _, rhs } = proc_ref else {
            panic!("x = ::bill was NOT evaluated as an assignment expression")
        };
        let Expression::Base { ref term, follow: _ } = **rhs else {
            panic!("::bill was NOT evaluated as a base expression")
        };
        match &term.elem {
            Term::GlobalIdent(field) => {
                if field != "bill" {
                    panic!("::bill did not eval bill as the global var to read")
                }
            }
            _ => panic!("::bill failed to eval :: as a global var read")
        }
    })
}
