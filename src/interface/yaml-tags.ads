--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package Yaml.Tags is
   Question_Mark    : constant Text.Reference; --  "?"
   Exclamation_Mark : constant Text.Reference; --  "!"
   Mapping          : constant Text.Reference; --  "!!map"
   Sequence         : constant Text.Reference; --  "!!seq"
   String           : constant Text.Reference; --  "!!str"
   Boolean          : constant Text.Reference; --  "!!bool"
   Null_Tag         : constant Text.Reference; --  "!!null"
private
   Question_Mark_Holder : constant Text.Constant_Instance :=
     Text.Hold ("?");
   Exclamation_Mark_Holder : constant Text.Constant_Instance :=
     Text.Hold ("!");
   Mapping_Holder : constant Text.Constant_Instance :=
     Text.Hold ("tag:yaml.org,2002:map");
   Sequence_Holder : constant Text.Constant_Instance :=
     Text.Hold ("tag:yaml.org,2002:seq");
   String_Holder : constant Text.Constant_Instance :=
     Text.Hold ("tag:yaml.org,2002:str");
   Boolean_Holder : constant Text.Constant_Instance :=
     Text.Hold ("tag:yaml.org,2002:bool");
   Null_Holder : constant Text.Constant_Instance :=
     Text.Hold ("tag:yaml.org,2002:null");

   Question_Mark : constant Text.Reference := Text.Held (Question_Mark_Holder);
   Exclamation_Mark : constant Text.Reference :=
     Text.Held (Exclamation_Mark_Holder);
   Mapping : constant Text.Reference := Text.Held (Mapping_Holder);
   Sequence : constant Text.Reference := Text.Held (Sequence_Holder);
   String : constant Text.Reference := Text.Held (String_Holder);
   Boolean : constant Text.Reference := Text.Held (Boolean_Holder);
   Null_Tag : constant Text.Reference := Text.Held (Null_Holder);
end Yaml.Tags;
