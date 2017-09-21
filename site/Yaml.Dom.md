---
layout: page
title: Yaml.Dom
permalink: /doc/Yaml.Dom/
weight: 7
api: True
---

The *Document Object Model* is a higher-level API which models the content of a
YAML document as graph structure. It is equivalent to the *Representation*
structure as defined in the YAML specification.

A YAML character stream can contain one or multiple documents.
`Yaml.Dom.Loading` provides the facilities to transform a stream into a single
document (in the case that multiple documents are not expected) or a list of
documents. For each document, you'll get a `Document_Reference` value.

Each document is reference-counted. When navigating it, you will get
`Node_Reference` values which, as long as they live, increase the reference
count of the document. As soon as all references to the document itself and its
nodes vanish, the document and all its nodes are being deallocated. The nodes
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

Dumping the DOM structure is currently not implemented.