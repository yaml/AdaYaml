--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

private package Yaml.Presenter.Analysis is
   --  this package analyzes scalars to check how they can be represented

   type Necessary_Quoting is (None, Only_In_Flow, Single, Double);

   type Scalar_Features is record
      Max_Word_Length        : Natural;
        --  the longest word in the string. a word is anything between two
        --  places where a folded scalar could be broken up.
      Max_Line_Length        : Natural;
        --  the longest line in the string. a line is anything between two
        --  line feeds.
      Single_Line_Length     : Natural;
        --  length of this string if it was rendered in a single line,
        --  excluding possibly necessary quoting signs.
      Quoting_Needed         : Necessary_Quoting;
        --  minimum quoting level of this scalar if rendered as flow scalar.
      Unquoted_Single_Line   : Boolean;
        --  true iff this string can be rendered unquoted as single-line scalar
      Folding_Possible       : Boolean;
        --  true iff this string can be represented as folded (block) scalar
      Contains_Non_Printable : Boolean;
        --  true iff this string contains non-printable characters
      Leading_Spaces         : Natural;
        --  number of leading spaces
      Trailing_Linefeeds     : Natural;
        --  number of trailing spaces
   end record;

   function Features (S : String) return Scalar_Features;
end Yaml.Presenter.Analysis;
