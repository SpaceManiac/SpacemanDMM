# DreamChecker

**DreamChecker** is a robust static analysis and typechecking engine for
DreamMaker, the scripting language of the [BYOND] game engine.

[BYOND]: https://secure.byond.com/

DreamMaker supports approximately four typechecking features:

* Field and method use with `.` will compile error if the annotated type of the
  left-hand side does not have a field or method matching the name on the
  right-hand side. Replacing `.` with `:` bypasses this.
* `for` loops where the automatically filter the input list using `istype` with
  the declared type of the loop variable, if present.
* `new()` calls which omit the type to construct use the declared type of the
  left-hand side of an assignment in which they are involved.
* `istype()` calls which omit the type to check against use the declared type
  of the variable passed to be checked.

However, even this limited support has problems:

* No way to specify the "value" type of associated lists, only the "key".
* Typecasts of all kinds (up, down, sideways) have no syntax whatsoever and are
  wholly unchecked.
* It is impossible to declare a return type for a procedure.
* Various apparently typelike annotations have no compile- or run-time effects
  despite being accepted (and ignored) by the compiler.
* There is no difference in type annotation between a variable intended to
  refer to object instances and typepaths. This leads to confusion, such as
  attempting to reference fields of a typepath as if it were an instance, which
  runtime errors.
* There is no notion of nullable or non-nullable types.
* There are no type annotations to represent primitives.

DreamChecker intends to address some or all of these shortcomings, integrating
ideas from TypeScript and family, including flow typing and automatically
determining appropriate type annotations in places where they are missing.
