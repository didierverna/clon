## Version 1.0b25
- More infrastructure overhaul.
- More flexible auto-detection of `termio`.
- The `exit` function is now deprecated in favor of `uiop:quit`.
- Improved support for interactive use.
- Support for dumping executables via ASDF's `program-op`.
- The documentation on application delivery has been expanded.
- Clon now depends on `named-readtables`.
- Bug fixes:
  * Fix some readtable problems with CCL, CLISP, and ECL.
  * Clon now compiles its `termio` support correctly with a C++ based ECL.
  * Fix one problem in path options'conversion protocol.
  * Other various bug fixes.

## Version 1.0b24
- Huge infrastructure overhaul (mostly done by Faré).
- The system and package name(s) have moved from a `com.dvlsoft` prefix to
  `net.didierverna`.

## Version 1.0b23
- Improved support for ABCL.
- Clon now switches to restricted mode in SBCL if the `CC` environment
  variable is not set.
- The `dump` macro now accepts a `&rest` argument that is passed on to the
  underlying implementation-specific dumping protocol.
- The distrivution now provides a `contrib` subdirectory.

## Version 1.0b22
- Support for LispWorks.
- Backward incompatible changes:
  * `*current-context*` has been renamed `*context*`.
  * `*default-synopsis*` has been renamed `*synopsis*`.
  * SBCL 1.0.57 is now required (new quitting protocol).
- Improved support for terminal autodetection and stream handling.

## Version 1.0b21
- Support for ACL.

## Version 1.0b20
- New, more user-friendly, error handler.
  Usage: `--clon-error-handler=interactive`.
- New option `--clon-lisp-information`.
- Command-line polling via `cmdline-options-p` and `cmdline-p`.
- Improved support for usage in interactive Lisp images (not dumped
  executables).
- Pre-load configuration facility through a global variable
  `cl-user::com.dvlsoft.clon.configuration`.
- `:restricted` mode (configuration option) for explicitly prohibiting the use
  IOCTL calls to communicate with TTYs.

## Version 1.0b19
- Several error handling related bug fixes.
- Switch from GNU GPL to BSD licencse.

## Version 1.0b18
- CFFI dependency for CLISP is now optional.

## Version 1.0b17
- Support for ABCL, modulo a dependency on CFFI.

## Version 1.0b16
- Support for CLISP, modulo a dependency on CFFI.

## Version 1.0b15
- Support for ECL.

## Version 1.0b12
- Support for CCL.

## Version 1.0b11
- Support for CMU-CL.

## Version 1.0b1
- First public release.
- Only supports SBCL.
