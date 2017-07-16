--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Streams;
with Yaml.Sources;
private with Yaml.Lexing;
private with Yaml.Events;
private with Yaml.Strings;
private with Yaml.Stacks;
private with Yaml.String_Sets;

package Yaml.Parsing is
   --  this package implements a parser that generates an event stream from a
   --  YAML characters stream source.

   type Parser is new Streams.Event_Stream with private;

   --  instructs the parser to parse the input provided by the given Source.
   --  the parser takes ownership  of the Source and will take care of
   --  deallocating it.
   procedure Set_Input (P : in out Parser; Input : Sources.Source_Access);

   --  instructs the parser to parse the input provided as String.
   procedure Set_Input (P : in out Parser; Input : String);

   --  retrieve the position where the lexer last tried to start reading a
   --  token. this function can be used when a Lexer_Error occurred.
   function Current_Lexer_Token_Start (P : Parser) return Mark;

   --  retrieve the position of the recently read character. this is useful
   --  when a Lexer_Error occurred.
   function Current_Input_Character (P : Parser) return Mark;

   --  retrieve the start position of the recently processed lexer token.
   --  useful when a Parser_Error occurred.
   function Recent_Lexer_Token_Start (P : Parser) return Mark;

   --  retrieve the end position of the recently processed lexer token.
   --  useful when a Parser_Error occurred.
   function Recent_Lexer_Token_End (P : Parser) return Mark;

private
   type Parser is new Streams.Event_Stream with null record;

   type Parser_Implementation;

   type Parser_State is access function
     (P : in out Parser_Implementation'Class; E : out Events.Event)
   return Boolean;

   subtype Indentation_Type is Integer range -2 .. Integer'Last;

   type Parsing_Level is record
      State : Parser_State;
      Indentation : Indentation_Type;
   end record;

   package Level_Stacks is new Yaml.Stacks (Parsing_Level);
   package Tag_Handle_Sets is new Yaml.String_Sets (Strings.Content);

   type Parser_Implementation is new Streams.Stream_Implementation with record
      Pool : Strings.String_Pool;
      L : Lexing.Lexer;
      Levels : Level_Stacks.Stack;
      Current : Lexing.Token;
      Cached : Events.Event;
      Tag_Handles : Tag_Handle_Sets.String_Set;
      Header_Props, Inline_Props : Events.Properties;
      Header_Start, Inline_Start : Mark;
      Block_Indentation : Indentation_Type;
   end record;
   type Parser_Implementation_Access is access all Parser_Implementation'Class;

   overriding procedure Close_Stream (Stream : in out Parser_Implementation);

   overriding procedure Fetch (Stream : in out Parser_Implementation;
                               E : out Events.Event);

   -----------------------------------------------------------------------------
   --  parser states
   -----------------------------------------------------------------------------

   --  starts the event stream by emitting a Stream_Start event
   function At_Stream_Start (P : in out Parser_Implementation'Class;
                             E : out Events.Event) return Boolean;

   --  final landing state. this just emits Stream_End events forever without
   --  reading anything.
   function At_Stream_End (P : in out Parser_Implementation'Class;
                           E : out Events.Event) return Boolean;

   --  state that expects a YAML document to start. reads directives.
   function Before_Doc (P : in out Parser_Implementation'Class;
                        E : out Events.Event) return Boolean;

   --  there may only be node properties and block scalar starts behind a '---'
   function After_Directives_End (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   --  state used when a document is started without explicit '---'. an explicit
   --  '---' is required for scalar documents.
   function Before_Implicit_Root (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   --  used when node properties occur at root levels. those *must* belong to
   --  a scalar which is an implicit map key and starts an implicit block map.
   function Require_Implicit_Map_Start (P : in out Parser_Implementation'Class;
                                        E : out Events.Event) return Boolean;

   --  used for parsing properties of a node.
   function Before_Node_Properties (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;

   --  state at the beginning of a block line. whether current implementation
   --  requires leaving block collections.
   function At_Block_Indentation (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   function At_Block_Indentation_Props (P : in out Parser_Implementation'Class;
                                        E : out Events.Event) return Boolean;

   --  state inside a block line where a new node may start in compact notation,
   --  e.g. after a `-` or `:`. this only sets the node start position and then
   --  advances to After_Block_Parent_Props while possibly reading node
   --  properties in between.
   function After_Block_Parent (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;

   --  this either starts compact nodes or advances to the next line
   function After_Block_Parent_Props (P : in out Parser_Implementation'Class;
                                      E : out Events.Event) return Boolean;

   --  used when there are node properties at the beginning of a line. in that
   --  case, the corresponding node must start at the same line.
   function Require_Inline_Block_Item (P : in out Parser_Implementation'Class;
                                       E : out Events.Event) return Boolean;

   --  in this state, the document's root node has been closed and the only
   --  valid lexer tokens are '...', '---' or the end of the stream.
   function Before_Doc_End (P : in out Parser_Implementation'Class;
                            E : out Events.Event) return Boolean;

   --  this state expects the next block sequence entry starting with a `- `.
   function In_Block_Seq (P : in out Parser_Implementation'Class;
                          E : out Events.Event) return Boolean;

   --  this state is used to emit the scalar which is the first implicit key of
   --  a block mapping. it is necessary because the previous state has emitted
   --  the mapping start event and cached the key scalar.
   function After_Implicit_Map_Start (P : in out Parser_Implementation'Class;
                                      E : out Events.Event) return Boolean;

   --  this state expects a `?` or an implicit mapping key
   function Before_Block_Map_Key (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   --  this state is used when Before_Block_Mapping_Key encounters node
   --  properties. after those, an implicit scalar mapping key must follow.
   function At_Block_Map_Key_Props (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;

   --  expects a `:` as mapping value indicator after an implicit key
   function After_Implicit_Key (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;

   --  expects a `:` as mapping value indicator after an explicit key
   function Before_Block_Map_Value (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;

   --  expects any item valid in flow mode
   function Before_Flow_Item (P : in out Parser_Implementation'Class;
                              E : out Events.Event) return Boolean;

   --  expects any item valid in flow mode after having read node properties.
   function Before_Flow_Item_Props (P : in out Parser_Implementation'Class;
                                    E : out Events.Event) return Boolean;

   --  expects either a `:` as mapping value indicator or `,` or `}` which both
   --  make the mapping value an implicit empty scalar.
   function After_Flow_Map_Key (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;

   --  expects either a `,`  or a `}`.
   function After_Flow_Map_Value (P : in out Parser_Implementation'Class;
                                  E : out Events.Event) return Boolean;

   --  expects either a `,` or a `]`.
   function After_Flow_Seq_Item (P : in out Parser_Implementation'Class;
                                 E : out Events.Event) return Boolean;

   --  expects either another key-value pair or `}` in which case the recently
   --  read comma is treated as trailing comma that does not start a new
   --  key-value pair.
   function After_Flow_Map_Sep (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;

   --  expects either another node as part of the sequence or `]` in which case
   --  the recently read comma is treated as trailing comma that does not start
   --  a new sequence entry.
   function After_Flow_Seq_Sep (P : in out Parser_Implementation'Class;
                                E : out Events.Event) return Boolean;

   --  the existence of node properties after a sequence separator (`,`) makes
   --  the comma non-trailing and forces the generation of another entry node,
   --  even if it is an implicit empty scalar.
   function After_Flow_Seq_Sep_Props (P : in out Parser_Implementation'Class;
                                      E : out Events.Event) return Boolean;

   --  expects the value of an implicit key-value pair inside a flow sequence.
   function Before_Pair_Value (P : in out Parser_Implementation'Class;
                               E : out Events.Event) return Boolean;

   --  used for emitting the cached scalar key of in implicit key-value pair
   --  in a flow sequence.
   function After_Implicit_Pair_Start (P : in out Parser_Implementation'Class;
                                       E : out Events.Event) return Boolean;

   --  used for emitting the mapping end event after an implicit key-value pair
   --  in a flow sequence.
   function After_Pair_Value (P : in out Parser_Implementation'Class;
                              E : out Events.Event) return Boolean;
end Yaml.Parsing;
