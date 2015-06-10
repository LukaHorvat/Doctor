# Start

Execution starts in the {Main.hs}(-~! !src/Main.hs -) file.
{The command line arguments provide the location of the git repository (or a git URL) and the location
of the documentation that needs to be compiled.}(-! !src/Main.hs manual; main; -)
(- break -)
Parsing
-------
(- file src/Parsing/Docs.hs -)
The work continues in the {parser}(-~! ! -).
The module defines parsers for documentation elements. It specifies how the
{reference syntax}(-! ! rawRef; -) looks and how the {tags}(-! ! tagged; -) work.
The output of the module is the {`Doc`}(-! ! data Doc; doc; -)
