Fmark
=====

Fmark (Friendly Markup) is a very simple markup language without
syntax and simple but sophisticated document styling, capable of
producing PDF and XML files.

Fmark does not have syntax, therefore, you do not have to add special,
strange characters, such as equals signs, underscores, or double
brackets ("=", "_", "[[") to write documents, as seen in many markup
languages.  Instead, Fmark relies merely on spacing, indentation, and
common punctuation (e.g., periods, exclamation and interrogation
marks) to reconstruct the structure of your document.  As a result,
your documents preserve their original formatting, thus avoiding the
recurring problem of having to manually remove the special characters
if you decide to move from one markup language to another.

Styles allow you to give meaning to your document without changing its
contents.  For example, you can assign parts of your document to
title, author names, date, abstract, and so on.  Creating styles is
also very simple because styles are simply documents written also in
Fmark.  As a result, you do not have to learn a different language to
be able to create your own style.

Fmark produces XML files and high-quality PDF documents, using LaTeX
as typeset engine.  More backends, for example for HTML, might be
added in the future if necessary.

Features:
- very simple markup language with styles
- automatic reconstruction of document structure
- no special syntactic characters to memorize or taint your document
- styles written in the same language
- write one document, experiment with multiple styles and formats

Example
-------

Let's try out a real example.  Copy the following text into a file
named 'example' or, if you have downloaded the source code or cloned
the repository, you can find this file in the 'doc/examples' directory.

    My first Fmark document
    José Lopes
    Sunday, Sep 4 2012

        Fmark (Friendly Markup) is a very simple markup language without
        syntax, capable of producing PDF and XML files, and very simple
        document but sophisticated styling.

    The first section

    Hello, welcome to Fmark.
    This is the first section of your document.

        The first subsection

        It is very simple to create subsections. No special characters are
        needed. Simply use indentation to indicate the beginning and end of
        your sections.

        The second subsection

        Another subsection with a different title. Again, very simple.

Copy the following text into 'example.style'.  Again, this file is
also available in the 'doc/examples' directory.

    Title
    Author
    Date

        Abstract.

OK! So the first document is the content and the second document is
the style.  As you can see, both documents have very similar
structure.  Now, let's ask Fmark to create a PDF using both documents

    $ fmark -s example.style -p example

This command produces a [PDF with styling](http://github.com/jabolopes/fmark/blob/master/doc/examples/example.pdf?raw=true)
using 'example' as the contents document and 'example.style' as the
style document.  After running this command, you should have a
'example.pdf' PDF file which you can open with your preferred PDF
viewer.

You can also create a [PDF without styling](http://github.com/jabolopes/fmark/blob/master/doc/examples/exampleNoStyle.pdf?raw=true)
by omitting the style argument

    $ fmark -p example

Formats
-------

Let's take a look at other formats and command line options.

You can change the output format to XML

    $ fmark -x example

and you can simply output the LaTeX formatting

    $ fmark -l example

These options can also be combined with styles if necessary.  Finally,
in case you forget any of these options you can always use the help
option

    $ fmark --help

Markup
------

Even though Fmark does not use syntactic characters, there are two
syntactic rules.  However, they simply follow the standard way of
writing documents.

The first rule is for paragraphs and headings: successive lines (i.e.,
lines not separated by blank lines) are joined together and if they
end in a punctuation symbol (e.g., period, exclamation or
interrogation marks), they are considered paragraphs.  Otherwise, they
are considered headings.  For example, the following is a paragraph

    Hi! This is a paragraph which
    happens to spans multiple lines.

but the following is a heading

    Hi! This is a heading which
    also spans multiple lines

The second rule is for sections and subsections: increase in
indentation causes a new subsection to be created which spans until
that indentation is decreased.  For example, the following is a
section with two subsections and a subsubsection.

    This is a section.

        This is a subsection.

            This is a subsubsection.

        Back to the subsection.

    And we are back on the first section.

Other rules will be added in the future for enumerations, lists, and
footnotes.

Installation from HackageDB
---------------------------

Installing from HackageDB is probably the best method. Use Cabal
install following these steps, making sure that your package list is
updated

    $ cabal update
    $ cabal install fmark

You can also checkout [Fmark on HackageDB](http://hackage.haskell.org/package/fmark).

Installation from source
------------------------

Download the source either by cloning the repository or by downloading
the source archive. Once you have the source, you can either use Cabal
(recommended) or the standard Haskell setup mechanism.

For Cabal, follow these steps to configure and build the application,
compile the documentation, and install the application.

    $ cabal configure
    $ cabal build
    $ cabal haddock --executables
    $ cabal install

For the standard Haskell setup, follow these steps

    $ runhaskell Setup configure
    $ runhaskell Setup build
    $ runhaskell Setup haddock --executables
    $ runhaskell Setup install