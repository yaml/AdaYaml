# ParserTools

This library provides several tools useful for writing lexers / parsers in Ada.

## Text

This library provides a reference-counted type named `Text.Reference` that
points to a UTF-8 encoded `String` of variable length. It is designed to be used
with parsers that produce text nodes / tokens. It is most efficient for parsers
that are operated in a stream-like manner, i.e. produced parser events are
processed when generated. It can also be used for a parser that builds up a full
AST. However, it is inefficient in scenarios where generated items go out of
scope in random order and new items are still generated while old items are
going out of order.

### Usage

`Text.Pool` provides a custom allocator. This allocator is optimized based on
the following assumptions:

 * The deallocation of text objects occurs in the same order as the allocation.
   This is no hard requirement, violations are tolerated, but may have an
   impact on performance.
 * Since the parser may be used through a C-level API, all Strings shall have an
   additional null terminator so that they can be efficiently exported to C.
   This terminator shall be hidden from the Ada view of the string value.
 * No manual memory management shall be necessary, both the Text objects and the
   Pool shall be reference-counted and automatically deallocated once the last
   reference vanishes.
 * Text objects shall be immutable once created.

To optimize performance, the allocator carves memory from pre-allocated chunks.
There is no freelist; instead, the dope vectors of the Ada String objects are
used to find the next free space. This way, adjacent free spaces can be
efficiently merged and thus, memory fragmentation is avoided.

The automatic memory management comes at the cost of every Text object holding
a reference to the Pool it was created with. This is necessary to make sure the
Pool only deallocates a memory chunk once the last Text object goes out of
scope.

## Lexer

`Lexer.Base` provides a base for a lexer. It is a translation of Nim's module
[lexbase][1]. It provides efficient buffering for the lexer source. It is build
for parsing languages which have more or less regular linebreaks. The lexer
only needs to check whether it needs to refill the buffer at linebreak
characters.

### Usage

After calling `Init` on the lexer, the next character can be queried with
`Next`. Whenever `Next` returns a linebreak character, the user must call
`Handle_LF` or `Handle_CR` (depending on the linebreak character) before
calling `Next` again. When the end of the input is reached, `Next` returns an
*End of Transmission* (`Character'Val (4)`) character. This character must not
be allowed to be a part of the input.

## License

[MIT](copying.txt)

 [1]: https://nim-lang.org/docs/lexbase.html
