--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Dom.Vectors;
with Yaml.Source;
with Yaml.Stream_Concept;

package Yaml.Dom.Loading is
   --  equivalent to the composition of the "Parse" and "Compose" steps in the
   --  YAML spec
   function From_Source (Input : Source.Pointer) return Document_Reference;
   function From_Source (Input : Source.Pointer) return Vectors.Vector;

   --  as above, but does not read from an external source but from a String
   function From_String (Input : String) return Document_Reference;
   function From_String (Input : String) return Vectors.Vector;

   generic
      with package Stream is new Stream_Concept (<>);
   package Stream_Loading is
      --  equivalent to the "Compose" step in the YAML spec. Expects the input
      --  to contain exactly one document, will raise a Compose_Error if the
      --  stream contains multiple documents.
      function Load_One (Input : in out Stream.Instance;
                         Pool  : Text.Pool.Reference :=
                           Text.Pool.With_Capacity (Text.Pool.Default_Size))
                         return Document_Reference;

      --  as above, but allows for multiple documents in a stream
      function Load_All (Input : in out Stream.Instance;
                         Pool  : Text.Pool.Reference :=
                          Text.Pool.With_Capacity (Text.Pool.Default_Size))
                         return Vectors.Vector
        with Post => Integer (Load_All'Result.Length) > 0;
   end Stream_Loading;
end Yaml.Dom.Loading;
