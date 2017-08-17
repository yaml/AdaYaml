---
layout: page
title: Yaml.Presenter
permalink: /doc/Yaml.Presenter/
weight: 6
api: True
---

The presenter is conceptually pretty similar to the parser, only that instead of
emitting events, it takes them as input, and instead of reading from sources, it
writes to destinations.

To use it, first call `Set_Output` once and then give it the events to be
presented.

## Destinations

Destinations derive from `Yaml.Destination.Instance`. Currently, the only
included destination is `Yaml.Destination.Text_IO`, but the will probably be
another one added mirroring `Yaml.Source.File`.

## Consuming Events

The presenter either takes one event after the other by calling the `Put`
procedure, or consumes a whole event stream with `Consume`. Be aware that
`Consume` may only be called with *complete* event streams, i.e. streams that
yield a `Stream_End` as last token.

With `Configure`, you can modify some output options such as the default line
length and the style of flow nodes. Note that the line length is more of a hint
than a hard restraint. After indentation, the content of a line may always have
half the length of the current maximum, which may result in lines longer than
the maximum if the indentation takes away more than half of that maximum
(e.g. in deeply nested structures). Also, non-breakable elements like tag URIs,
if longer than the maximum, will violate that maximum.

## Error Handling

The presenter can raise these two exceptions:

 * `Stream_Error` if the event stream formed by the given events is malformed.
 * `Destination_Error` if writing to the destination fails (for example, if
   you write directly to a String and the output would be longer than the
   provided String).

Additionally, the standard `Storage_Error` may be raised if no more memory is
available.