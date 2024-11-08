# Copyright (C) 2010, 2011, 2015, 2024 Didier Verna

This file is part of Clon.

Copying and distribution of this file, with or without modification,
are permitted in any medium without royalty provided the copyright
notice and this notice are preserved.


# Introduction
In the Lisp family of languages, one of the key components is the so-called
REPL, the Read-Eval-Print Loop, which blends the runtime, compilation and
development phases together, allowing for a very high level of interaction
with the program. The importance of this paradigm explains why it is still
rare to find "standalone" applications written in Lisp: most Lisp application
clients are lispers themselves, and they prefer to live in the REPL.

On the other hand, many contemporary Common Lisp compilers provide ways of
creating standalone Common Lisp programs, using either shebang technology or
by directly dumping executables. This process is getting easier to achieve
every day. For instance, with SBCL, all it takes is one function call
(save-lisp-and-die) and one command-line option to the compiler (--script).

The (big) size of a Lisp image has been considered a showstopper in creating
standalone applications for a long time, but nowadays, it doesn't really
matter anymore (if you are not convinced, just figure out the average size of
an application bundle under Mac OS X). This, along with the fact that Common
Lisp compilers can generate very efficient code today, makes the perspective
of standalone Common Lisp applications very tempting again.

When it comes to preserving interaction with the user, one of the key
components in a standalone application is the handling of the command-line.
Clon is a library designed to do just that.


# Overview
Clon provides traditional features one might expect from a command-line
options management library, but what makes it somewhat unique in its
conception is that it has been designed with both the application developer
and the application user in mind: many aspects of the command-line management
usually imposed by the program are in fact under the control of the
application user. The idea is that all Clon-enabled applications behave in a
uniform way, and that it is the user who gets to decide, not the developper.
The most important features of Clon are listed below.

## Command-Line Syntax
Clon imposes a particular yet flexible syntax for the command-line. Options
have short and/or long names, beginning with either one or two dashes. Options
may take an argument. In that case, the argument may be optional or mandatory.
Option names may be abbreviated and one-character options may be grouped
together in a single call. There is an additional call syntax for Boolean or
Boolean-based options. Finally, the command-line may have a "postfix", that
is, a non-option part.

## Option Types
Clon comes with a set of predefined option types, including "flags" (option
without arguments), "switches" (Boolean options), simple Lisp objects, simple
strings, enumerations, pathnames etc. Clon is also extensible: it is possible
to define new option types, either from scratch or by subclassing an existing
one.

## Value Retrieval
The traditional approach to command-line analysis is to process it
sequentially, and Clon lets you do that. However, Clon also provides an
explicit retrieval mechanism by which you can directly request the value of a
specific option, regardless of its position, or even its presence on the
command-line. An option's value can be retrieved from different sources: a
command-line argument, a "fallback" value (for optional arguments), a default
value or an environment variable associated with the option. The retrieval
process is completely automated, hence removing this burden from the
developer's shoulders.

## Error Management
Another cumbersome task already taken care of is error management. Clon
provides two built-in error management schemes, in case something is wrong on
the command-line. The simplest one is to quit the application with an
informative error message. The other one is to enter an interactive dialog
with the user, in which it is possible to fix the problem (correct a typo,
discard an option call, provide a missing argument or value etc.). A unique
feature of Clon is that the application user may choose his or her preferred
error management scheme.

## Help String Contents
The "help string" is typically what you expect from the output of a --help
option. Maintaining an up-to-date help string is a very boring task, so Clon
completely automates this for you (another burden removed from the developer's
shoulders). However, the application programmer still has control over the
help string's contents, notably the order in which options are displayed, and
also by having the possibility of grouping options together and inserting
arbitrary text in the output.

# Help String Format
Clon also completely automates the help string typesetting process. For
instance, it can automatically format the output for 80 columns, but it is
also aware of the tty geometry and will use it when appropriate. Clon also
supports help string "fontification" (or "highlighting") through ISO/IEC 6429
SGR escape sequences on tty's that support it. Another unique feature of Clon
is that the exact format and highlight specification for the help string is
under the control of the application user, via the notion of "theme". Clon
comes with a set of predefined themes (for instance, standard, with heavy
fontification, refcard for quick reference etc.) and application users can
define their own. As a matter of fact, a theme not only controls the format
and highlight of the help string, but also its contents.
