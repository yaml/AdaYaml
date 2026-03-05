---
layout: page
title: Yaml.Dom
permalink: /doc/Yaml.Dom/
weight: 7
api: True
---

## Overview

The *Document Object Model* is a higher-level API which models the content of a
YAML document as graph structure. It is equivalent to the *Representation*
structure as defined in the YAML specification.

## Loading

A YAML character stream can contain one or multiple documents.
`Yaml.Dom.Loading` provides the facilities to transform a stream into a single
document (in the case that multiple documents are not expected) or a list of
documents. For each document, you'll get a `Document_Reference` value.

## Data Layout & Handling

Each document is reference-counted. When navigating it, you will get
`Node_Reference` values which, as long as they live, increase the reference
count of the document. As soon as all references to the document itself and its
nodes vanish, the document and all its nodes will be deallocated. The nodes
are not individually reference-counted because a YAML graph may contain cycles.

In the DOM, anchors and aliases have been resolved. That means that if in the
YAML input, a node had an anchor and there was an alias somewhere that
referenced that anchor, in the DOM, these two places will point to the same node
in memory. You can check whether two `Node_Reference` values point to the same
node with the `Same_Node` function, which only compares the memory location.

Nodes can be checked for equivalence with the `"="` function, which compares
their tags and their content. That comparison may be costly because it needs to
break cycles; however, it short-circuits as soon as a difference is found.

The content of collection nodes is managed by the two packages
`Yaml.Dom.Sequence_Data` and `Yaml.Dom.Mapping_Data`. They mimic the API of the
standard packages `Ada.Containers.Vectors` and `Ada.Containers.Hashed_Maps`,
respectively and are actually backed by those structures with all implications
about algorithmic complexity of the provided subprograms. The indirection is
necessary for providing the reference-counted types.

You can programmatically create document objects and nodes with the constructor
subprograms in `Yaml.Dom`. To create a node, you always need an existing
`Document_Reference`. That node will be owned by this document and must not be
put into a different document.

The document and node objects preserve some of the representation information
from the event stream. Note that according to the YAML specification, this
representational information must not be used when processing the input data. It
is however convenient to tweak the output you will get when dumping and leads
to preservation of most of the document's style when dumping a loaded document.

## Dumping

`Yaml.Dom.Dumping` provides facilities to serialize a document to an event
stream or directly dump it to a destination. You can either dump one or
multiple documents at once. The `To_Event_Queue` functions create a
`Yaml.Events.Queue` object which contains the sequence of generated events.
Events cannot be produced one-by-one because anchors are only added when the
second reference to the same node is encountered. Every dumping method produces
a complete stream including a `Stream_Start` and a `Stream_End` event.

## Code examples

Typical code for loading the standard input into a DOM structure would look like
this:

{% highlight ada %}
with Ada.Text_IO;

with Yaml.Source.Text_IO;
with Yaml.Dom.Loading;
with Yaml.Dom.Node;

procedure Load_Dom is
   use type Dom.Node_Kind;

   Input : Source.Pointer :=
     Yaml.Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   Document : Yaml.Dom.Document_Reference :=
     Yaml.Dom.Loading.From_Source (Input);
begin
   Ada.Text_IO.Put_Line ("Root node is a " & Document.Root.Value.Kind'Img);
end Load_Dom;
{% endhighlight %}

Typical code for dumping a document to standard output would look like this:

{% highlight ada %}
with Ada.Text_IO;

with Yaml.Destination.Text_IO;
with Yaml.Dom.Node;
with Yaml.Dom.Dumping;
with Yaml.Dom.Mapping_Data;
with Yaml.Dom.Sequence_Data;

procedure Dump_Dom is
   Document : Yaml.Dom.Document_Reference := Yaml.Dom.New_Document;
   Root : Yaml.Dom.Node_Reference := Document.New_Mapping;
   Key  : Yaml.Dom.Node_Reference := Document.New_Scalar ("a sequence");
   Seq  : Yaml.Dom.Node_Reference := Document.New_Sequence;
begin
   Seq.Value.Data.Items.Append (Document.New_Scalar ("one"));
   Seq.Value.Data.Items.Append (Document.New_Scalar ("two"));
   Seq.Value.Data.Items.Append (Document.New_Scalar ("three"));
   Root.Value.Data.Pairs.Insert (Key, Seq);
   Document.Set_Root (Root);
   Yaml.Dom.Dumping.Dump
     (Document, Yaml.Destination.Text_IO.As_Destination
                  (Ada.Text_IO.Standard_Output));
end Dump_Dom;
{% endhighlight %}

This will dump the following YAML character stream:

{% highlight yaml %}
a sequence:
  - one
  - two
  - three
{% endhighlight %}