--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Transformator;
with Yaml.Stream_Concept;
private with Ada.Containers.Indefinite_Vectors;

generic
   with package Stream_Impl is new Stream_Concept (<>);
package Yaml.Transformation is
   type Instance is limited new Refcount_Base with private;
   type Reference (Data : not null access Instance) is tagged private;

   overriding procedure Finalize (Object : in out Instance);

   function Transform (Original : Stream_Impl.Reference) return Instance;
   function Transform (Original : Stream_Impl.Reference) return Reference;

   function Next (Object : in out Instance) return Event;

   --  takes ownership of the given pointer.
   procedure Append (Object : in out Instance;
                     T : not null Transformator.Pointer);
private
   type Reference (Data : not null access Instance) is
     new Ada.Finalization.Controlled with null record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   subtype Not_Null_Pointer is not null Transformator.Pointer;

   package Transformator_Vectors is new Ada.Containers.Indefinite_Vectors
     (Positive, Not_Null_Pointer, Transformator."=");

   type Instance is limited new Refcount_Base with record
      Original : not null access Stream_Impl.Instance;
      Transformators : Transformator_Vectors.Vector;
   end record;
end Yaml.Transformation;
