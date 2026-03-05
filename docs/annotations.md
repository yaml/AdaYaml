---
layout: page
title: Transformation Annotations
permalink: /trans/
weight: 8
---

## Introduction

AdaYaml contains a prototype implementation of a proposed
*transformation extension*. This extension assigns semantics to certain
YAML 1.3 annotations which allows the user to specify data transformations
within YAML files.

The extension is implemented in AdaYaml at the event stream level, meaning
that it takes an input event stream and generates an output event stream.
The implementation is available from the `yaml-annotation_processor.gpr`
project file. Documentation is sparse currently, refer to `yaml-to_events.adb`
from the `yaml-utils.gpr` project for example usage. Since this prototype is
currently under heavy development, it is discouraged to be used in production
as the API is completely unstable and the specification is due to change even
after a first complete version of the prototype is finished.

The rest of this document is written in an implementation-indipendent view and
is supposed to become the extension's formal specification eventually.

# Specification

## Overview

The *transformation extension* specifies a number of annotations that hold
the semantics of transforming the structure of the YAML nodes they are applied
to. The goals of this specification are:

 * To provide an alternative to the [merge key][1] which was specified as
   optional extension for YAML 1.1 and has not been updated since.
 * To provide an alternative to preprocessing YAML data with templating
   engines such as Jinja, as it is done for example in Ansible and SaltStack.
   A solution integrated with YAML is expected to provide a better user
   experience since it understands the YAML structure which it is generating
   or transforming and does not need to operate on character level, where
   one needs to care about proper whitespace handling. 
 * To provide some features that are somewhat regularly asked for on
   StackOverflow, such as injecting externally-defined values into a YAML
   stream, scalar interpolation, including one YAML document in another,
   defining variables which are not directly part of the content, and so on.

Overall, this extension pays tribute to the fact that YAML, though conceived
as serialization language, nowadays is first and foremost used as configuration
language. The specified annotations are designed for the use-case of humans
writing YAML documents and are unlikely to be of any use when serializing data
to YAML.

This extension is *not* part of any YAML specification. A YAML processor is
*not* required to implement any part of it. The extension is based on YAML
*annotations* which are currently an RFC that may or may not be integrated into
the upcoming 1.3 version of the YAML specification; therefore, the existence
of an implementation of YAML annotations is a prerequisite of this
specification.

## API and Integration into the YAML Loading Process

An implementation of this extension shall be called an
**Transformation Processor**. This document will simply use the word
*Processor* when referring to it. A Processor shall take an event stream as
input. An event stream is what the YAML 1.2 specification calls an *Event Tree*
or a *Serialization* and is created by parsing a YAML *Presentation*. The
output of a Processor is also an event stream; however, it shall not contain
any *transformation annotations*, i.e. annotations defined by this
specification.

When loading a YAML character stream with the Transformation Processor enabled,
the loading process takes the following form (modified from the original
process in the YAML specification):

<svg width="750" height="220">
  <use xlink:href="/img/annotation-loading.svg#diagram"></use>
</svg>

As you can see, *transformation annotations* are removed in the
*annotation processing* step when moving from the **Raw Serialization** to the
**Serialization** with processed transformations. The transformation processor
shall also remove *external aliases*, i.e. aliases that do not refer to an
anchored node within the current YAML event stream. These are allowed in
YAML 1.3 so that users can refer to values from the environment. Since
the transformation processor may need to resolve aliases as part of the
processing of some annotations, it will need those referenced external values
as additional input anyway, so it is defined to integrate them into the output
event stream so that the output is guaranteed to be self-contained.

## Alias Handling

If an alias occurs somewhere in a structure that is being transformed, it shall
always be handled as if it was being replaced by the node structure it refers
to. For example, if an annotation defines that it must be applied to a sequence
node, it may also be applied to an alias node that references a sequence node.

If an alias occurs somewhere in the input which resolves to a node structure
that is not yet part of the output (for example because its referenced node
is provided externally or inside an annotated node which removes the referenced
node from the output), this alias shall be replaced in the output by the node
structure it references. Further aliases to that node structure shall stay
aliases as they can now reference the node structure that has been part of the
output as replacement for the first alias.

<figure class="example">
  <div class="left">
    <p>Raw</p>
{% highlight yaml %}
- @@concat &a [foo, bar]
- *a
- *a
{% endhighlight %}
  </div><div class="right">
    <p>Processed</p>
{% highlight yaml %}
- foobar
- &a [foo, bar]
- *a
{% endhighlight %}
  </div>
  <figcaption>Alias replaced by referenced node since original node is not part
              of the output</figcaption>
</figure>

## Alias Resolution Scopes

The Processor needs to resolve aliases before they are required to be resolved
by the specification, i.e. before the *compose* step. Since some annotations
will use aliases as variables (for example loop variables), the Processor needs
more sophisticated alias resolution rules than the YAML specification provides.
These rules are defined in this section.

A **scope** shall be a collection of ordered YAML events that provides a
*symbol list* which can be used to resolve aliases to anchored events that are
part of this collection. This extension defines four different *scope types*:

 * **External Scope** is the scope for target values that are not part of or
   generated by the YAML stream. This scope is necessary to create the ability
   to refer to values of the parser environment, for example values provided by
   the code calling the YAML loader or the system environment.

<figure class="example">
  <div class="left">
    <p>Raw</p>
{% highlight yaml %}
- &a scalar
- *b
{% endhighlight %}
  </div><div class="right">
    <p>Processed</p>
{% highlight yaml %}
- &a scalar
- &b externally provided value
{% endhighlight %}
  </div>
  <figcaption>The alias <code>*b</code> does not map to any anchor inside the
             YAML stream and must therefore be externally provided.</figcaption>
</figure>

 * **Stream Scope** is the scope for values contained or generated within the
   YAML stream whose anchors are valid for the rest of the YAML stream
   beginning at the position where they are created. This scope is used to
   communicate values between YAML documents within the stream. The primary
   use-case of this scope is defining aliasable values without adding them to
   the document body via the `@@vars` annotation.

<figure class="example">
  <div class="left">
    <p>Raw</p>
{% highlight yaml %}
--- @@vars
a: foobar
---
*a: *a
{% endhighlight %}
  </div><div class="right">
    <p>Processed</p>
{% highlight yaml %}
---
&a foobar: *a
{% endhighlight %}
  </div>
  <figcaption>The alias <code>*a</code> maps to an anchor which has been placed
              into the stream scope by <code>@@vars</code>. Its first occurrence
              must be replaced by its referenced node because it is not part of
              the output yet.</figcaption>
</figure>

 * **Document Scope** is the scope for values whose anchors are valid until the
   document ends. Node start events (i.e. scalars and mapping / sequence /
   annotation start events) with anchors in the YAML stream end up in a document
   scope.

<figure class="example">
  <div class="left">
    <p>Raw</p>
{% highlight yaml %}
- &a scalar
- *a
{% endhighlight %}
  </div><div class="right">
    <p>Processed</p>
{% highlight yaml %}
- &a scalar
- *a
{% endhighlight %}
  </div>
  <figcaption><code>*a</code> is a reference to an anchored node as per YAML
              specification. Because the target node is part of the output,
              the alias does not need to be processed.</figcaption>
</figure>

 * **Local Scope** is a scope for values which are only valid for a part of the
   document. Local scopes are the only scope type that can be nested. Local
   scopes are used for things like loop variables.

<figure class="example">
  <div class="left">
    <p>Raw</p>
{% highlight yaml %}
--- @@for(i, [1, 2, 3])
- id: *i
{% endhighlight %}
  </div><div class="right">
    <p>Processed</p>
{% highlight yaml %}
---
- id: 1
- id: 2
- id: 3
{% endhighlight %}
  </div>
  <figcaption><code>*i</code> is in the local scope and only valid inside the
              node annotated with <code>@@for</code>.</figcaption>
</figure>

The lifetime of the scope types is a total order, i.e. the current document
scope always outlives any contained local scope, a stream scope always outlives
any contained document scope, and an external scope always outlives any
contained stream scope.

At any location of a YAML stream where an alias event may occur, there is
always exactly one valid scope of each external scope, stream scope, and
document scope type. Additionally, there may be any number of valid local
scopes, which are ordered with the outermost local scope being the first one.

The alias is resolved to the target anchored node as follows: The local scopes
are searched in reverse order, then the document scope, the stream scope and
the external scope. The first scope that can resolve the alias will provide its
target. An unresolvable alias event shall result in a Processor error.

## Annotations

### ``@@concat``

`@@concat` must be applied on a sequence. This sequence shall either contain
only sequences or only scalars. If the sequence contains scalars, the annotated
node shall be replaced by a concatenation of all contained scalars. If the
sequence contains sequences, the annotated node shall be replaced by a sequence
of all items contained in the original sequences, with the items of the first
given sequence coming first in their original order and so on. ``@@concat``
applied on an empty sequence shall be replaced by an empty sequence.

The resulting node shall have the same anchor as the annotation. If the
annotation has an explicit tag, the resulting node shall have that tag. Else,
the resulting node shall have the `?` non-specific tag.

<figure class="example">
  <div class="left">
    <p>Raw</p>
{% highlight yaml %}
- @@concat [foo, bar]
- !numbers @@concat [[1, 2], [3], [4, 5]]
- &a @@concat &b []
{% endhighlight %}
  </div><div class="right">
    <p>Processed</p>
{% highlight yaml %}
- foobar
- !numbers [1, 2, 3, 4, 5]
- &a []
{% endhighlight %}
  </div>
  <figcaption><code>@@concat</code> applied to various sequences</figcaption>
</figure>

### ``@@for``

``@@for`` can be applied on any node. It takes exactly 2 parameters. The first
parameter shall be a scalar which contains a valid anchor name. Thes second
parameter shall be a sequence.

``TODO``

### ``@@vars``

``@@vars`` must be applied to a mapping which is the root of a YAML document.
This mapping shall only have scalar keys. For each key-value pair, the value
shall be placed in the current *stream scope* and the key shall be put in the
symbol table of that stream scope so that it resolves to its value. The whole
document which contains the annotated root node shall be removed from the
YAML stream. If it is the only document in the stream, the Processor shall
raise an error.


 [1]: http://yaml.org/type/merge.html

