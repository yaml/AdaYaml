--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Ada.Unchecked_Deallocation;

package body Yaml.Transformator.Annotation.Concatenation is
   procedure Put (Object : in out Instance; E : Event) is
   begin
      Object.State.all (Object, E);
   end Put;

   function Has_Next (Object : Instance) return Boolean is
     (Object.Current_Exists);

   function Next (Object : in out Instance) return Event is
   begin
      if not Object.Current_Exists then
         raise Constraint_Error with "no event to retrieve";
      end if;
      Object.Current_Exists := False;
      return Object.Current;
   end Next;

   function New_Concatenation (Pool : Text.Pool.Reference;
                               Context : Events.Context.Instance)
                               return not null Pointer is
     (new Instance'(Transformator.Instance with Pool => Pool,
                    Context => Context, others => <>));

   procedure Initial (Object : in out Instance'Class; E : Event) is
   begin
      if E.Kind /= Annotation_Start then
         raise Stream_Error with
           "unexpected token (expected annotation start): " & E.Kind'Img;
      end if;
      Object.Current.Start_Position := E.Start_Position;
      Object.State := After_Annotation_Start'Access;
   end Initial;

   procedure After_Annotation_Start (Object : in out Instance'Class; E : Event) is
   begin
      if E.Kind /= Annotation_End then
         raise Annotation_Error with
           "@concat does not take any parameters.";
      end if;
      Object.State := After_Annotation_End'Access;
   end After_Annotation_Start;

   procedure After_Annotation_End (Object : in out Instance'Class; E : Event) is
   begin
      if E.Kind /= Sequence_Start then
         raise Annotation_Error with
           "@concat must be applied on a sequence.";
      end if;
      Object.State := After_List_Start'Access;
   end After_Annotation_End;

   procedure After_List_Start (Object : in out Instance'Class; E : Event) is
   begin
      case E.Kind is
         when Scalar =>
            Object.Builder :=
              new Text.Builder.Reference'(Text.Builder.Create (Object.Pool));
            Object.Builder.Append (E.Content.Value);
            Object.State := After_String'Access;
         when Sequence_Start =>
            Object.Depth := 1;
            Object.Current := Event'(Kind => Sequence_Start,
                                     Collection_Style => Any, others => <>);
            Object.Current_Exists := True;
            Object.State := In_Sequence'Access;
         when others =>
            raise Annotation_Error with
              "@concat requires a list of scalars or sequences";
      end case;
   end After_List_Start;

   procedure In_Sequence (Object : in out Instance'Class; E : Event) is
   begin
      case E.Kind is
         when Sequence_Start =>
            Object.Depth := Object.Depth + 1;
            Object.Current := E;
            Object.Current_Exists := True;
         when Sequence_End =>
            Object.Depth := Object.Depth - 1;
            if Object.Depth = 0 then
               Object.State := After_Sequence'Access;
            else
               Object.Current := E;
               Object.Current_Exists := True;
            end if;
         when others =>
            Object.Current := E;
            Object.Current_Exists := True;
      end case;
   end In_Sequence;

   procedure After_Sequence (Object : in out Instance'Class; E : Event) is
   begin
      case E.Kind is
         when Sequence_Start =>
            Object.Depth := 1;
            Object.State := In_Sequence'Access;
         when Sequence_End =>
            Object.Current := E;
            Object.Current_Exists := True;
            Object.State := After_List_End'Access;
         when others =>
            raise Annotation_Error with
              "illegal element in sequence, expected another sequence: " &
              E.Kind'Img;
      end case;
   end After_Sequence;

   procedure After_String (Object : in out Instance'Class; E : Event) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Text.Builder.Reference, Builder_Pointer);
   begin
      case E.Kind is
         when Scalar =>
            Object.Builder.Append (E.Content.Value);
         when Sequence_End =>
            Object.Current := (Kind => Scalar, Scalar_Style => Any,
                               Scalar_Properties => <>,
                               Start_Position => Object.Current.Start_Position,
                               End_Position => E.End_Position,
                               Content => Object.Builder.Lock);
            Object.Current_Exists := True;
            Free (Object.Builder);
            Object.State := After_List_End'Access;
         when others =>
            raise Annotation_Error with
              "illegal element in sequence, expected another scalar: " &
              E.Kind'Img;
      end case;
   end After_String;

   procedure After_List_End (Object : in out Instance'Class; E : Event) is
   begin
      raise Constraint_Error with
        "unexpected input to @concat (already finished)";
   end After_List_End;
begin
   Map.Include ("concat", New_Concatenation'Access);
end Yaml.Transformator.Annotation.Concatenation;
