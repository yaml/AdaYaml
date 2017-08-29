# ParserText

This library provides a reference-counted type named `Text.Reference` that
points to a UTF-8 encoded `String` of variable length. It is designed to be used
with parsers that produce text nodes / tokens. It is only efficient for parsers
that are operated in a stream-like manner, i.e. produced parser events are
processed when generated. For a parser that builds up a full AST, this package
is inefficient and should not be used.

## Features

ParserText provides a `Text.Pool` with a custom allocator. This allocator is
optimized based on the following assumptions:

 * The deallocation of text objects occurs in the same order as the allocation.
   This is no hard requirement, violations are tolerated.
 * Since the parser may be used through a C-level API, all Strings shall have
   an additional null terminator so that they can be efficiently exported to C.
 * No manual memory management shall be necessary, both the Text objects and the
   Pool shall be reference-counted and automatically deallocated once the last
   reference vanishes.
 * Text objects are immutable once created.

To optimize performance, the allocator carves memory from pre-allocated chunks.
There is no freelist; instead, the dope vectors of the Ada String objects are
used to find the next free space. This way, adjacent free spaces can be
efficiently merged and thus, memory fragmentation is avoided.

The automatic memory management comes at the cost of every Text object holding
a reference to the Pool it was created with. This is necessary to make sure the
Pool only deallocates a memory chunk once the last Text object goes out of
scope.

## License

[MIT](copying.txt)