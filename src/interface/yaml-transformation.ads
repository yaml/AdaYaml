--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Transformator;
with Yaml.Stream_Concept;
private with Ada.Containers.Indefinite_Vectors;

generic
   with package Stream_Impl is new Stream_Concept (<>);
package Yaml.Transformation is
   type Instance is limited new Refcount_Base with private;
   type Instance_Access is access all Instance;
   type Reference is tagged private;
   type Accessor (Data : not null access Instance) is null record with
     Implicit_Dereference => Data;

   overriding procedure Finalize (Object : in out Instance);

   function Transform (Original : Stream_Impl.Reference) return Instance;
   function Transform (Original : Stream_Impl.Reference) return Reference;

   function Value (Object : Reference) return Accessor;

   function Next (Object : in out Instance) return Event;

   --  takes ownership of the given pointer.
   procedure Append (Object : in out Instance;
                     T      : not null Transformator.Pointer);
private
   type Reference is new Ada.Finalization.Controlled with record
      Data : not null Instance_Access;
   end record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   subtype Not_Null_Pointer is not null Transformator.Pointer;

   package Transformator_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Not_Null_Pointer, Transformator."=");

   type Instance is limited new Refcount_Base with record
      Original       : Stream_Impl.Reference;
      Transformators : Transformator_Vectors.Vector;
   end record;
end Yaml.Transformation;
