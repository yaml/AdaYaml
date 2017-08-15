---
layout: page
title: Documentation
permalink: /doc/
weight: 3
---

## Compiler Support

Since AdaYaml is written in Ada 2012, you can currently only use it in
conjunction with the [GNAT][1] compiler; there are no other compilers available
supporting Ada 2012. Since GNAT is the only viable compiler anyway, the AdaYaml
source makes use of some GNAT-specific features.

Moreover, AdaYaml integrates with other tools of the GNAT environment,
particularly the build system [GPRbuild][2]. You can easily define AdaYaml as
dependency of your project using GPRbuild, and you can easily install AdaYaml on
your system using GPRinstall. This is all described in detail in the GPRbuild
documentation.

## API Concepts

AdaYaml uses reference counting for managing resources allocated on the heap.
Since reference counting makes it necessary to use smart pointers instead of raw
access types, AdaYaml uses naming conventions derived from [Rosen '95][3] to
communicate the nature of these smart pointer types:

 * A type named `Reference` always is a reference to a reference-counted heap
   object. If and how the referenced object is directly accessible depends on
   the type. If the type has the `Implicit_Dereference` aspect, it dereferences
   to an access type pointing to the heap object. Copying a value of such a type
   always results in the reference being copied.
 * A type named `Instance` typically allocates its content on the stack
   (if used as a stack variable), but may contain references to heap values. If
   a `Reference` type exists wrapping an `Instance` type, using the `Reference`
   type is generally optional. If a subroutine, particularly in another package,
   may take co-ownership of an instance, it will require a `Reference` as
   parameter type.
 * A type named `Pointer` is a raw access type to heap memory. A subroutine
   taking a value of such a type is guaranteed to take ownership of the value
   and deallocate it once it is not needed anymore.

The API has been designed so that a user does never need to actively deallocate
anything, minimizing the danger of memory leaks.

## Example

This example code reads a file (or stdin, if no file is given on the command
line) and writes the resulting YAML events to stdout.

{% highlight ada %}
with Ada.Text_IO;
with Ada.Command_Line;

with Yaml.Source.Text_IO;
with Yaml.Source.File;
with Yaml.Parser;

procedure To_Events is
   Input : Yaml.Source.Pointer;
   P     : Yaml.Parser.Instance;
   Cur   : Yaml.Event;
begin
   --  Input is either stdin or a file given as command line argument
   if Ada.Command_Line.Argument_Count = 0 then
      Input := Yaml.Source.Text_IO.As_Source (Ada.Text_IO.Standard_Input);
   else
      Input := Yaml.Source.File.As_Source (Ada.Command_Line.Argument (1));
   end if;

   --  The parser takes ownership of the input and will deallocate it.
   --  Note that an input must be set before events can be queried.
   P.Set_Input (Input);

   loop
      --  Fetch the next event from the parser
      Cur := P.Next;
      --  Print the event to stdout
      Ada.Text_IO.Put_Line (Yaml.To_String (Cur));
      --  If the event was a Stream_End, exit the loop (emitting Stream_End
      --  is the parser's way of saying "there are no more events".)
      exit when Cur.Kind = Yaml.Stream_End;
   end loop;
end To_Events;
{% endhighlight %}

 [1]: http://gnuada.sourceforge.net
 [2]: https://docs.adacore.com/gprbuild-docs/html/gprbuild_ug.html
 [3]: http://dl.acm.org/citation.cfm?id=224131