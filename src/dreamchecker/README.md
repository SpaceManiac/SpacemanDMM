# DreamChecker

**DreamChecker** is a robust whole-program static analysis and type checking
engine for DreamMaker, the scripting language of the [BYOND] game engine.

[BYOND]: https://secure.byond.com/

## Running DreamChecker

DreamChecker can be obtained with `cargo build -p dreamchecker` or from the
[releases] page.

DreamChecker should be run from within the DM project directory. It will
automatically detect the `.dme` file, parse it, and issue diagnostics.
DreamChecker will exit with a non-zero status code if it discovers any
diagnostics, making it suitable for use in continuous integration environments.

[releases]: https://github.com/SpaceManiac/SpacemanDMM/releases

## Diagnostics

In addition to the simple inline diagnostics discovered by the [parsing suite],
DreamCheckers's whole-program analysis can find problems such as:

[parsing suite]: ../dreammaker/#diagnostics

* Unknown `set` setting names.
* Undefined types on unused variables.
* Keyword arguments being passed to procs which do not accept them.
* Calling procs with non-keyword arguments following keyword arguments.
* Proc overrides which are missing keyword arguments that their parents are
  called with.
* Declaring vars with `/list` in unusual positions, e.g.
  `var/atom/list/movable/L`.
* Use of `src` in global `/proc`s where it is guaranteed to be `null`.
* Calling the parent proc `..()` when no such parent exists.
* Accesses like `L[1].foo` and `foo().bar` wherein `.` acts like `:` instead.
  * List accesses perform lookups according to the type appended to `/list`,
    e.g. with `var/list/obj/L`, the type of `L[1]` will be `/obj` and a lookup
    of `L[1].name` will not generate a warning.
  * Proc calls will obey the [return type](#return-type) annotation if present.

## Extensions

DreamChecker also adds additional typing features to the language through a
series of extensions, described below.
Because DreamMaker does not recognize these extensions, it is recommended that
you use the preprocessor flag `SPACEMAN_DMM` to determine whether they should
be enabled:

```dm
#ifdef SPACEMAN_DMM
	#define RETURN_TYPE(X) set SpacemanDMM_return_type = X
	#define SHOULD_CALL_PARENT(X) set SpacemanDMM_should_call_parent = X
	#define UNLINT(X) SpacemanDMM_unlint(X)
	#define SHOULD_NOT_OVERRIDE(X) set SpacemanDMM_should_not_override = X
	#define FINAL_VAR var/final
#else
	#define RETURN_TYPE(X)
	#define SHOULD_CALL_PARENT(X)
	#define UNLINT(X) X
	#define SHOULD_NOT_OVERRIDE(X)
	#define FINAL_VAR var
#endif
```

### Return type

Use `set SpacemanDMM_return_type = expression` to set a return type expression
for a proc. The return type can take the forms:

* `/typepath` - a raw typepath. The return type of the proc is the type named.
* `param` - a typepath given as a parameter, for procs which return an instance
  of the passed-in type.
* `param.type` - the static type of a passed-in parameter, for procs which
  return their input or otherwise another value of the same type.
* `param[_].type` - the static type of a passed-in parameter, with one level
  of `/list` stripped, for procs which select one item from a list. The `[_]`
  may be repeated to strip more levels of `/list`.

### Should call parent

Use `set SpacemanDMM_should_call_parent = 1` to enable a diagnostic on children
of the proc it is set on which do not contain any `..()` parent calls. This can
help with finding situations where a signal or other important handling in the
parent proc is being skipped. Child procs may set this setting to `0` instead
to override the check.

### Should not override

Use `set SpacemanDMM_should_not_override = 1` to raise a warning for any child
procs that override this one, regardless of if it calls parent or not.
This functions in a similar way to the `final` keyword in some languages.

### Final variables

Use the above definition of FINAL_VAR to declare vars as `final`, `var/final/foo` such that overriding their value isn't permitted by types that inherit it.
```
/a/type
  FINAL_VAR/foo = somevalue
```
