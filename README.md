Clon is a library for command-line options management. It is intended to ease
the creation of standalone Common Lisp applications by providing a powerful
and uniform command-line options interface. The most important features of
Clon are the following.

  - From the application programmer's point of view: centralized command-line
  options specification and management, including automatic generation of help
  strings, conversion from command-line / defaults / fallbacks / environment
  variables to application-level option values, global or on-demand option
  retrieval, and extensibility (the programmer can define his own option
  types).

  - From the application user's point of view: uniform command-line option
  syntax across all Clon applications, customization of the help strings
  layout (with optional ISO6429 coloring on terminals that support it),
  automatic completion of abbreviated option names and short/long/pack syntax.
