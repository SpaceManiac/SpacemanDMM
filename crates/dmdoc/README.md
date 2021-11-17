# dmdoc

**dmdoc** is a documentation generator for DreamMaker, the scripting language
of the [BYOND] game engine. It produces simple static HTML files based on
documented files, macros, types, procs, and vars.

[BYOND]: https://secure.byond.com/

If dmdoc is run in a Git repository, web links to source code are placed next
to item headings in the generated output; otherwise, file and line numbers are
shown but are not linked.

## Running dmdoc

dmdoc can be obtained with `cargo build -p dmdoc` or from the [releases] page.

dmdoc should be run from within the DM project directory. It will
automatically detect the `.dme` file, parse it, and generate documentation.

Documentation and resource files will be saved into the `dmdoc` directory. The
resulting documentation will be suitable for uploading to any web server. It
may be desirable to delete this directory if it exists before running dmdoc, to
remove old files.

[releases]: https://github.com/SpaceManiac/SpacemanDMM/releases

## Documenting code

Types, macros, vars, and procs can be documented using any of four different
doc comment styles. Both block and line comments are supported. Documentation
blocks may target their enclosing item or the following item.

```dm
/// This comment applies to the following macro.
#define BLUE rgb(0, 255, 0)

/** Block comments work too. */
/obj
    var/affinity = BLUE  //! Enclosing comments follow their item.

/proc/chemical_reaction()
    /*! Block comments work too. */
```

Types with only a one-line note and no documented procs or vars do not get
their own `.html` page.

### Modules

Enclosing-item comments which are not inside any element are applied to the
current file. Files which have such comments, or which contain any documented
macros, are added to the modules tree. If a file is in the modules tree, any
documented types in that file will appear in its entry.

The summary sections of module pages are sorted in line number order, with
file-level documentation interspersed with the summary lines of documented
items.

Markdown and .txt files inside the `code` directory will also be rendered and added to
the modules tree.

The contents of `code/README.md` will be used in the main page of the generated
documentation.

### Crosslinks

You can link inside any doc comment or markdown file to another documented piece of code.

This is done via a crosslink, in the form `[path_to_link_to]`.

Valid forms of crosslinks:
```dm
[DEFINE_NAME]
[/path_to_object]
[/path_to_object/proc/foo]
[/path_to_object/var/bar]
```

You can also customize the link text that appears. This is by prepending the custom link text in brackets, such as: `[some define][DEFINE_NAME]`.

### Titles

The title of a documentation entry is determined by whichever is set first:
* A `# Title` set at the top of a doc block, if present.
* The type's `name` var if present and not [disabled in config].
* The last component of the typepath.

Here's an example of setting a manual title for an object:
```dm
/**
  * # Fubar
  */
/obj/foo
```

This will cause the `/obj/foo` to have the title `Fubar` in the generated documentation. The typepath will still show up at the top of the details page.

[disabled in config]: ../../CONFIGURING.md#dm-doc
