# Doctor
Smart documentation

What
----
A documentation 'generator'. It takes files written in Markdown and produced pretty HTMLs out of them.
Key feature is that the Markdown is enhanced with a feature that lets you tag parts of the documentation to pieced of
code in your codebase.
The benefits are that it's much easier to document your codebase this way (the "what goes where" aspect) and that
there's automatic checking for stale documentation being done.

When new code is committed, everything that pointed to the things that changed gets flagged as stale. It's then tthe programmers
job to go through those points and either approve the docs (saying they're still valid) or ammend them.

How
---
The "atoms" which you can reference aren't lines of code, but actual, language aware, units such as functions, classes, values, etc.
For that purpose we introduce language providers. These are plugins that a file and a string id, interpret that id
as a class/function/whatever name and return back the part of that file that contains the unit.
The Doctor keeps track of what the provider returned last time when asked about the same id. If there are differences, the docs
become stale.
