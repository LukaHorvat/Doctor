# Doctor
Smart documentation

What
----
A documentation 'generator'. It takes files written in Markdown and produces pretty HTMLs out of them.
The key is a feature that lets you tag parts of the documentation to pieces of code in your codebase.
The benefit is that it's much easier to document your codebase this way (the "what goes where" aspect) and that
there's automatic checking for stale documentation being done.

When new code is committed, everything that pointed to the things that changed gets flagged as stale. Then it's the programmers
job to go through that report and either approve the docs (saying they're still valid) or amend them.

How
---
The "atoms" which you can reference aren't lines of code, but actual, language aware units such as functions, classes, values, etc.
This is where we introduce language providers. These are plugins that take a file and a string id, interpret that id
as a class/function/whatever name and return back the part of that file that contains that specific code unit.
The Doctor keeps track of what the provider returned last time when asked about the same id. If there are differences, the docs
become stale.
