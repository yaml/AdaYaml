--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Text.Builder.Unicode is
   procedure Append (Object : in out Reference;
                     Value : Strings_Edit.UTF8.Code_Point) is
   begin
      if Object.Next + 3 > --  maximum UTF-8 encoded character length is 4
        System.Storage_Elements.Storage_Offset (Object.Buffer.all'Last) then
         Grow (Object, 4);
      end if;
      Strings_Edit.UTF8.Put (Object.Buffer.all, Positive (Object.Next), Value);
   end Append;
end Text.Builder.Unicode;
