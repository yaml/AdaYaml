--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

package body Yaml.Presenter.Analysis is
   function Features (S : String) return Scalar_Features is
      Pos : Positive := S'First;
      Cur : Character;
      Cur_Word_Length : Natural := 0;
      Cur_Line_Length : Natural := 0;

      --  states

      procedure Read_Beginning (Ret : out Scalar_Features) is
      begin
         if S'Length = 0 then
            Ret := (Max_Word_Length => 0, Max_Line_Length => 0,
                    Single_Line_Length => 0, Quoting_Needed => Single,
                    Unquoted_Single_Line => True, Folding_Possible => True,
                    Contains_Non_Printable => False,
                    Leading_Spaces => 0, Trailing_Linefeeds => 0);
         else
            Ret := (Max_Word_Length        => 0,
                    Max_Line_Length        => 0,
                    Single_Line_Length     => 0,
                    Quoting_Needed         => None,
                    Unquoted_Single_Line   => True,
                    Folding_Possible       => True,
                    Contains_Non_Printable => False,
                    Leading_Spaces         => 0,
                    Trailing_Linefeeds     => 0);
            Cur := S (Pos);
            if Cur = ' ' then
               Ret.Quoting_Needed := Single;
               loop
                  Ret.Leading_Spaces := Ret.Leading_Spaces + 1;
                  Cur_Word_Length    := Cur_Word_Length + 1;
                  Cur_Line_Length    := Cur_Line_Length + 1;
                  Pos := Pos + 1;
                  exit when Pos > S'Last;
                  Cur := S (Pos);
                  exit when Cur /= ' ';
               end loop;
            end if;
         end if;
      end Read_Beginning;

      procedure Read_Word (Ret : in out Scalar_Features) is
      begin
         if Cur = '#' then
            Ret.Quoting_Needed :=
              Necessary_Quoting'Max (Ret.Quoting_Needed, Single);
            Ret.Unquoted_Single_Line := False;
         end if;
         loop
            case Cur is
               when ':' =>
                  Ret.Single_Line_Length := Ret.Single_Line_Length + 1;
                  if Pos < S'Last and then S (Pos + 1) = ' ' then
                     Ret.Quoting_Needed :=
                       Necessary_Quoting'Max (Ret.Quoting_Needed, Single);
                     Ret.Unquoted_Single_Line := False;
                     Pos := Pos + 1;
                     return;
                  end if;
               when Character'Val (7) .. Character'Val (9) =>
                  Ret.Quoting_Needed := Double;
                  Ret.Single_Line_Length := Ret.Single_Line_Length + 2;
                  Ret.Contains_Non_Printable := True;
               when '"' =>
                  if Pos = S'First then
                     Ret.Quoting_Needed :=
                       Necessary_Quoting'Max (Ret.Quoting_Needed, Single);
                     Ret.Single_Line_Length := Ret.Single_Line_Length + 2;
                     Ret.Unquoted_Single_Line := False;
                  else
                     Ret.Single_Line_Length := Ret.Single_Line_Length + 1;
                  end if;
               when others =>
                  Ret.Single_Line_Length := Ret.Single_Line_Length + 1;
            end case;
            Pos := Pos + 1;
            exit when Pos > S'Last;
            Cur := S (Pos);
            exit when Cur in ' ' | Character'Val (10);
            Cur_Word_Length := Cur_Word_Length + 1;
            Cur_Line_Length := Cur_Line_Length + 1;
         end loop;
      end Read_Word;

      procedure Read_Newlines (Ret : in out Scalar_Features) is
      begin
         Ret.Max_Line_Length :=
           Natural'Max (Ret.Max_Line_Length, Cur_Line_Length);
         Cur_Line_Length := 0;
         Ret.Max_Word_Length :=
           Natural'Max (Ret.Max_Word_Length, Cur_Word_Length);
         Cur_Word_Length := 0;
         loop
            Ret.Trailing_Linefeeds := Ret.Trailing_Linefeeds + 1;
            Pos := Pos + 1;
            exit when Pos > S'Last;
            Cur := S (Pos);
            exit when Cur /= Character'Val (10);
         end loop;
         if Pos <= S'Last then
            Ret.Trailing_Linefeeds := 0;
         end if;
      end Read_Newlines;

      procedure Read_Space_After_Word (Ret : in out Scalar_Features) is
      begin
         Cur_Line_Length := Cur_Line_Length + 1;
         Ret.Single_Line_Length := Ret.Single_Line_Length + 1;
         Pos := Pos + 1;
         if Pos > S'Last then
            Ret.Quoting_Needed :=
              Necessary_Quoting'Max (Ret.Quoting_Needed, Single);
            Ret.Unquoted_Single_Line := False;
            Cur_Word_Length := Cur_Word_Length + 1;
            Cur_Line_Length := Cur_Line_Length + 1;
            return;
         end if;
         Cur := S (Pos);
         if Cur in ' ' | Character'Val (10) then
            Cur_Word_Length := Cur_Word_Length + 1;
            while Cur = ' ' loop
               Cur_Word_Length := Cur_Word_Length + 1;
               Cur_Line_Length := Cur_Line_Length + 1;
               Ret.Single_Line_Length := Ret.Single_Line_Length + 1;
               Pos := Pos + 1;
               exit when Pos > S'Last;
            end loop;
         else
            Ret.Max_Word_Length :=
              Natural'Max (Ret.Max_Word_Length, Cur_Word_Length);
            Cur_Word_Length := 0;
         end if;
      end Read_Space_After_Word;
   begin
      return Ret : Scalar_Features do
         Read_Beginning (Ret);
         if Pos <= S'Last then
            loop
               Read_Word (Ret);
               exit when Pos > S'Last;
               loop
                  case Cur is
                  when ' ' => Read_Space_After_Word (Ret);
                  when Character'Val (10) => Read_Newlines (Ret);
                  when others => null; --  never happens
                  end case;
                  exit when Pos > S'Last or else
                    not (Cur in ' ' | Character'Val (10));
               end loop;
               exit when Pos > S'Last;
            end loop;
         end if;
      end return;
   end Features;
end Yaml.Presenter.Analysis;
