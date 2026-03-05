---
layout: page
title: Text
permalink: /doc/Text/
weight: 4
api: True
---

The `Text` package comes from the separate [ParserText][1] project. It is not
YAML-specific, but has been written to support AdaYaml. The goals of the `Text`
package are:

 * To implement reference-counting on strings so that they can be passed safely
   and efficiently as part of the `Yaml.Event` type.
 * To allocate strings in a way that they are directly exportable to C
   (by writing a terminator character directly behind the content characters).
 * To implement efficient memory management for the typical use-case of parsers
   (generating strings in a first-allocated-first-deallocated fashion while
   avoiding memory fragmentation).

To be able to implement this, `Text` uses quite some pretty low-level techniques
like writing a custom dope vector in front of each String's content. The
resulting code is pretty compiler-specific and most probably will only work with
GNAT for the forseeable future (even if other compilers start to support
Ada 2012).

## Using `Text.Reference`

To query the value of a `Text.Reference`, use the `Value` function which returns
an `Accessor` that implicitly dereferences to an `UTF_8_String` access. The
value is not modifyable. The initial value of a `Text.Reference` is the empty
string; you can always safely call `Value` since the underlying pointer may
never be `null`.

## Creating a `Text.Reference`

If you want to create own `Text.Reference` values, you need a
`Text.Pool.Reference`. The underlying object needs a lot of memory, so the
reference is not initialized by default; you need to call `Create` on it once.
A pool lives on until the last `Text.Reference` created by it and all
`Text.Pool.Reference` objects vanish. If you are using a `Yaml.Parser.Instance`,
you can query its pool with the `Pool` function to avoid using two pools.

If you want to create a `Text.Reference` by subsequently appending content, use
`Text.Builder.Reference`.

## Providing `Text.Reference` Constants

If you want to have package-level constants of `Text.Reference`, instead of
allocating them in a pool, create a `Constant_Instance` object with the function
`Hold` and then generate a reference from it with the function `Held`. Example:

{% highlight ada %}
with Text;

package My_Package is
   My_Constant : constant Text.Reference;
private
   My_Constant_Holder : constant Text.Constant_Instance := Text.Hold ("value");
   My_Constant : constant Text.Reference := Text.Held (My_Constant_Holder);
end My_Package;
{% endhighlight %}

 [1]: https://github.com/flyx/ParserText