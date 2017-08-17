---
layout: page
title: Yaml.Parser
permalink: /doc/Yaml.Parser/
weight: 5
api: True
---

AdaYaml's parser has a quite simple interface. Basically, you call one of the
`Set_Input` procedures to tell the parser where to read the YAML character
stream from and then start querying events with `Next`.

## Sources

Input can be either a `String` or a polymorphic `Yaml.Source.Instance'Class`.
AdaYaml provides two implementations for sources:

 * `Yaml.Source.File.Instance` can read any non-special files from the file
   system, i.e. files for which the size of the content can be queried via
   `Ada.Directiories`. Use this implementation whenever possible, it is the
   faster one.
 * `Yaml.Source.Text_IO.Instance` for reading any `Ada.Text_IO.File_Type`. Use
   this only when necessary, e.g. for reading standard input or a special file
   like `/dev/urandom`.

You can provide an own implementation of `Yaml.Source.Instance` for sources not
covered by AdaYaml, e.g. a network stream.

## Querying Events

Parsing is lazy. The next event is typically only parsed from the input when you
query it. This means that if the input contains any syntax errors, `Next` will
only raise an exception after all parseable events in front of the syntax error
have been yielded.

By convention, the parser has finished after it emits a `Stream_End` token.
Subsequent calls to `Next` will always yield another `Stream_End` token.

## Error Handling

A call to `Next` can typically yield two exceptions:

 * `Lexer_Error` is raised when an error in the lexical structure of the input
   has been discovered. The raised exception always has a message attached that
   explains what went wrong. If you want to display the error to the user after
   catching it, you may use `Current_Lexer_Token_Start` and
   `Current_Input_Character` to query the start and end position of the current
   entity the lexer failed to tokenize.
 * `Parser_Error` is raised when an error in the syntactical structure of the
   input has been discovered, and is also accompanied by an error message. You
   can query the current lexical token which caused the error by calling
   `Recent_Lexer_Token_Start` and `Recent_Lexer_Token_End`.

In addition, standard Ada exceptions that can be raised are:

 * `Storage_Error` when running out of memory. This can occur since creating
   text content values for the event may cause a memory allocation.
 * Any error which originates from the source implementation. For example, the
   I/O exceptions from `Ada.Text_IO` may be raised when parsing from a
   `Yaml.Source.Text_IO.Instance`.
