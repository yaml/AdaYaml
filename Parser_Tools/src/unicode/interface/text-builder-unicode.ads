--  part of ParserTools, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Strings_Edit.UTF8;

package Text.Builder.Unicode is
   procedure Append (Object : in out Reference;
                     Value : Strings_Edit.UTF8.Code_Point);
end Text.Builder.Unicode;
