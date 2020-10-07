# SpacemanDMM suite configuration

All 'opinionated' errors in the suite have an associated configuration option to redefine their Severity or to disable them completely.

Errors that would fail to compile in the DM compiler or will always cause a runtime cannot be disabled (eg: bad keyword arguments).

As of writing, all front ends have the functionality to accept a configuration file.  They default to using `SpacemanDMM.toml` located in the same directory as the `.dme` file.

## Configuration Options

All of these options can be set to the following:

* `error`, `errors` - Severity::Error
* `warning`, `warnings` - Severity::Warning
* `info`, `infos` - Severity::Info
* `hint`, `hints` - Severity::Hint
* `disabled`, `false`, `off` - Disables it completely
* `unset`, not present in the config file - Uses the default

### Diagnostics

The `[diagnostics]` section has the following options:

Raised by DreamChecker:

* `disabled_directive` - Raised when attempting to disable a `set SpacemanDMM_*` directive that cannot be disabled
* `sets_directive_twice` - Raised when a directive is set twice in the same proc
* `invalid_lint_directive_value` - Raised when attempting to set a directive value to something other than `1`, `0`, `TRUE`, `FALSE`
* `invalid_set_value` - Raised on invalid values used with builtin set directives
* `unknown_linter_setting` - Raised when setting a `SpacemanDMM_*` directive that DreamChecker doesn't implement
* `override_missing_keyword_arg` - Raised when proc overrides are missing keyword arguments
* `must_not_override` - `SpacemanDMM_should_not_override` directive
* `must_call_parent` - `SpacemanDMM_should_call_parent` directive
* `final_var` - `SpacemanDMM_final` var type
* `private_proc` - `SpacemanDMM_private_proc` directive
* `protected_proc` - `SpacemanDMM_protected_proc` directive
* `private_var` - `SpacemanDMM_private` var type
* `protected_var` - `SpacemanDMM_protected` var type
* `must_be_pure` - `SpacemanDMM_should_be_pure` directive
* `must_not_sleep` - `SpacemanDMM_should_not_sleep` directive
* `redefined_proc` - `SpacemanDMM_can_be_redefined` directive
* `ambiguous_in_lhs` - Raised on ambiguous operations on the left hand side of an `in` operation
* `no_typehint_implicit_new` - Raised on the use of `new` where no typehint is avaliable
* `field_access_static_type` - Raised on using `.field_name` on a variable with no typehint
* `proc_call_static_type` - Raised on using `.proc_name()` on a variable with no typehint
* `proc_has_no_parent` - Raised on calling `..()` in a proc with no parent.
* `no_operator_overload` - Raised on using a unary operator on a non-primative that doesn't define it's own override, eg `somemob++`
* `unreachable_code` - Raised on finding code that can never be executed
* `control_condition_static` - Raised on a control condition such as `if`/`while` having a static condition such as `1` or `"string"`
* `if_condition_determinate` - Raised on if condition being always true or always false
* `loop_condition_determinate` - Raised on loop condition such as in `for` being always true or always false

Raised by Lexer:

* `integer_precision_loss` - Raised where an integer is out of integer range and is implicitly formatted as a float

Raised by Parser:

* `var_in_proc_parameter` - Raised where `var/` is used in proc arguments
* `static_in_proc_parameter` - Raised where `static/` is used in proc arguments
* `semicolon_in_proc_parameter` - Raised where `;` is used in proc arguments
* `in_precedes_as` - Raised where `input()` calls are using `as` after `in` which DM silently ignores
* `tmp_no_effect` - Raised where local vars are defined as `tmp` which has no effect
* `final_no_effect` - Raised where local vars are defined as `SpacemanDMM_final` which has no effect
* `as_local_var` - Raised where local vars are defined using the `as Foo` syntax which has no effect

Raised by PreProcessor:

* `duplicate_include` - Raised where the same file is included twice
* `macro_redefined` - Raised where a macro is defined a second time
* `macro_undefined_no_definition` - Raised where a macro is undefined where no such macro is defined

Raised by Object Tree:

* `override_precedes_definition` - Raised where a proc is overridden prior to its definition in the include order, see: http://www.byond.com/forum/post/2441385

### Display

The `[display]` section has the following options:

* `error_level` - Sets the level at which errors are registered instead of being ignored

### Language server

The `[langserver]` section has the following options:

* `dreamchecker` - Set to `true` to run dreamchecker within the language server.

### Code standards

These are extremely opinionated lint warnings and as such default to disabled

The `[code_standards]` section has the following options:

* `disallow_relative_proc_definitions` - Raised on relative pathed proc definitions
* `disallow_relative_type_definitions` - Raised on relative pathed subtype defintions

### DM Doc

The `[dmdoc]` section has the following options:

* `use_typepath_names` - Set to `true` to have dmdoc use the true typepath name instead of the value of the `name` var for types

## Example

```toml
[display]
error_level = "hint"

[langserver]
dreamchecker = true

[diagnostics]
duplicate_include = "error"
macro_redefined = "off"
```
