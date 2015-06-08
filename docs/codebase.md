# Start

Execution starts in the {Main.hs}(-~src/Main.hs -) file.
{The command line arguments provide the location of the git repository (or a git URL) and the location
of the documentation that needs to be compiled.}(- src/Main.hs manual; main; -)
(- break -)
Parsing
-------
The work continues in the {parser}(- ~src/Parsing/Docs.hs -).
The module defines parsers for documentation elements. It specifies how the
{reference syntax}(- src/Parsing/Docs.hs rawRef; -) looks and how the {tags}(- src/Parsing/Docs.hs tagged; -) work.
The output of the module is the {`Doc`}(- src/Parsing/Docs.hs data Doc; doc; -)
