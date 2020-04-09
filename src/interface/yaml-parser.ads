--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Source;
with Text.Pool;
private with Ada.Finalization;
private with Yaml.Lexer;
private with Yaml.Stacks;
private with Yaml.Text_Set;

package Yaml.Parser is
   --  this package implements a parser that generates an event stream from a
   --  YAML characters stream source.

   type Instance is limited new Refcount_Base with private;
   type Instance_Access is access all Instance;
   subtype Class is Instance'Class;
   type Reference is tagged private;
   type Accessor (Data : not null access Instance) is limited null record with
     Implicit_Dereference => Data;

   function Value (Object : Reference) return Accessor;

   type Warning_Handler is limited interface;
   procedure Wrong_Yaml_Version (Handler : in out Warning_Handler;
                                 Version : String) is abstract;
   procedure Unknown_Directive (Handler      : in out Warning_Handler;
                                Name, Params : String) is abstract;

   function New_Parser return Reference;

   --  instructs the parser to parse the input provided by the given Source.
   --  the parser takes ownership  of the Source and will take care of
   --  deallocating it.
   procedure Set_Input (P : in out Instance; Input : Source.Pointer);

   --  instructs the parser to parse the input provided as String.
   procedure Set_Input (P : in out Instance; Input : String);

   --  register a handler whose subroutines are called when the corresponding
   --  warning condition is met. May be set to null in which case no handler
   --  is called (default behavior).
   procedure Set_Warning_Handler
     (P : in out Instance; Handler : access Warning_Handler'Class);

   --  retrieve the position where the lexer last tried to start reading a
   --  token. this function can be used when a Lexer_Error occurred.
   function Current_Lexer_Token_Start (P : Instance) return Mark;

   --  retrieve the position of the recently read character. this is useful
   --  when a Lexer_Error occurred.
   function Current_Input_Character (P : Instance) return Mark;

   --  retrieve the start position of the recently processed lexer token.
   --  useful when a Parser_Error occurred.
   function Recent_Lexer_Token_Start (P : Instance) return Mark;

   --  retrieve the end position of the recently processed lexer token.
   --  useful when a Parser_Error occurred.
   function Recent_Lexer_Token_End (P : Instance) return Mark;

   function Next (P : in out Instance) return Event;

   function Pool (P : Instance) return Text.Pool.Reference;
private
   overriding procedure Finalize (P : in out Instance);

   type Reference is new Ada.Finalization.Controlled with record
      Data : not null access Instance;
   end record;

   overriding procedure Adjust (Object : in out Reference);
   overriding procedure Finalize (Object : in out Reference);

   type State_Type is access function
     (P : in out Class; E : out Event) return Boolean;

   subtype Indentation_Type is Integer range -2 .. Integer'Last;

   type Parsing_Level is record
      State : State_Type;
      Indentation : Indentation_Type;
   end record;

   package Level_Stacks is new Yaml.Stacks (Parsing_Level);
   package Tag_Handle_Sets is new Yaml.Text_Set (Text.Reference);

   type Instance is limited new Refcount_Base with record
      Pool : Text.Pool.Reference;
      L : Lexer.Instance;
      Levels : Level_Stacks.Stack;
      Current : Lexer.Token;
      Cached : Event;
      Tag_Handles : Tag_Handle_Sets.Reference;
      Header_Props, Inline_Props : Properties;
      Header_Start, Inline_Start : Mark;
      Block_Indentation : Indentation_Type;
      Handler : access Warning_Handler'Class;
   end record;
   type Instance_Pointer is access Instance;

   -----------------------------------------------------------------------------
   --  parser states
   -----------------------------------------------------------------------------

   --  starts the event stream by emitting a Stream_Start event
   function At_Stream_Start (P : in out Class; E : out Event) return Boolean;

   --  final landing state. this just emits Stream_End events forever without
   --  reading anything.
   function At_Stream_End (P : in out Class; E : out Event) return Boolean;

   --  state that expects a YAML document to start. reads directives.
   function Before_Doc (P : in out Class; E : out Event) return Boolean;

   --  there may only be node properties and block scalar starts behind a '---'
   function After_Directives_End (P : in out Class;
                                  E : out Event) return Boolean;

   --  state used when a document is started without explicit '---'. an explicit
   --  '---' is required for scalar documents.
   function Before_Implicit_Root (P : in out Class;
                                  E : out Event) return Boolean;

   --  used when node properties occur at root levels. those *must* belong to
   --  a scalar which is an implicit map key and starts an implicit block map.
   function Require_Implicit_Map_Start (P : in out Class;
                                        E : out Event) return Boolean;

   --  used for parsing properties of a node.
   function Before_Node_Properties (P : in out Class;
                                    E : out Event) return Boolean;

   --  state at the beginning of a block line. whether current implementation
   --  requires leaving block collections.
   function At_Block_Indentation (P : in out Class;
                                  E : out Event) return Boolean;

   function At_Block_Indentation_Props (P : in out Class;
                                        E : out Event) return Boolean;

   --  state inside a block line where a new node may start in compact notation,
   --  e.g. after a `-`, `?` or `:` after an explicit key.
   function After_Compact_Parent (P : in out Class;
                                  E : out Event) return Boolean;

   --  this either starts compact nodes or advances to the next line
   function After_Compact_Parent_Props (P : in out Class;
                                        E : out Event) return Boolean;

   --  similar to After_Compact_Parent, but disallows compact notation. This
   --  is used after a `:` of an implicit key.
   function After_Block_Parent (P : in out Class;
                                E : out Event) return Boolean;

   --  this either starts compact nodes or advances to the next line
   function After_Block_Parent_Props (P : in out Class;
                                      E : out Event) return Boolean;

   --  used when there are node properties at the beginning of a line. in that
   --  case, the corresponding node must start at the same line.
   function Require_Inline_Block_Item (P : in out Class;
                                       E : out Event) return Boolean;

   --  in this state, the document's root node has been closed and the only
   --  valid lexer tokens are '...', '---' or the end of the stream.
   function Before_Doc_End (P : in out Class;
                            E : out Event) return Boolean;

   --  this state expects the next block sequence entry starting with a `- `.
   function In_Block_Seq (P : in out Class;
                          E : out Event) return Boolean;

   --  this state is used to emit the scalar which is the first implicit key of
   --  a block mapping. it is necessary because the previous state has emitted
   --  the mapping start event and cached the key scalar.
   function After_Implicit_Map_Start (P : in out Class;
                                      E : out Event) return Boolean;

   --  this state expects a `?` or an implicit mapping key
   function Before_Block_Map_Key (P : in out Class;
                                  E : out Event) return Boolean;

   --  this state is used when Before_Block_Mapping_Key encounters node
   --  properties. after those, an implicit scalar mapping key must follow.
   function At_Block_Map_Key_Props (P : in out Class;
                                    E : out Event) return Boolean;

   --  expects a `:` as mapping value indicator after an implicit key
   function After_Implicit_Key (P : in out Class;
                                E : out Event) return Boolean;

   --  expects a `:` as mapping value indicator after an explicit key
   function Before_Block_Map_Value (P : in out Class;
                                    E : out Event) return Boolean;

   --  expects a newline after any node in a block context
   --  (not used after implicit block mapping keys)
   function Before_Block_Indentation (P : in out Class;
                                      E : out Event) return Boolean;

   --  expects any item valid in flow mode
   function Before_Flow_Item (P : in out Class;
                              E : out Event) return Boolean;

   --  expects any item valid in flow mode after having read node properties.
   function Before_Flow_Item_Props (P : in out Class;
                                    E : out Event) return Boolean;

   --  expects either a `:` as mapping value indicator or `,` or `}` which both
   --  make the mapping value an implicit empty scalar.
   function After_Flow_Map_Key (P : in out Class;
                                E : out Event) return Boolean;

   --  expects either a `,`  or a `}`.
   function After_Flow_Map_Value (P : in out Class;
                                  E : out Event) return Boolean;

   --  expects either a `,` or a `]`.
   function After_Flow_Seq_Item (P : in out Class;
                                 E : out Event) return Boolean;

   --  expects either another key-value pair or `}` in which case the recently
   --  read comma is treated as trailing comma that does not start a new
   --  key-value pair.
   function After_Flow_Map_Sep (P : in out Class;
                                E : out Event) return Boolean;

   --  expects either another node as part of the sequence or `]` in which case
   --  the recently read comma is treated as trailing comma that does not start
   --  a new sequence entry.
   function After_Flow_Seq_Sep (P : in out Class;
                                E : out Event) return Boolean;

   --  the existence of node properties after a sequence separator (`,`) makes
   --  the comma non-trailing and forces the generation of another entry node,
   --  even if it is an implicit empty scalar.
   function After_Flow_Seq_Sep_Props (P : in out Class;
                                      E : out Event) return Boolean;

   --  used for emitting an empty scalar in the case that a pair with empty key
   --  is encountered.
   function At_Empty_Pair_Key (P : in out Class; E : out Event) return Boolean;

   --  expects the value of an implicit key-value pair inside a flow sequence.
   function Before_Pair_Value (P : in out Class;
                               E : out Event) return Boolean;

   --  used for emitting the cached scalar key of in implicit key-value pair
   --  in a flow sequence.
   function After_Implicit_Pair_Start (P : in out Class;
                                       E : out Event) return Boolean;

   --  used for emitting the mapping end event after an implicit key-value pair
   --  in a flow sequence.
   function After_Pair_Value (P : in out Class;
                              E : out Event) return Boolean;

   --  expects either another part of the parameter list or `)` in which case
   --  the recently read comma is treated as trailing comma that does not start
   --  a new parameter.
   function After_Param_Sep (P : in out Class; E : out Event)
                          return Boolean;

   --  the existence of node properties after a sequence separator (`,`) makes
   --  the comma non-trailing and forces the generation of another param node,
   --  even if it is an implicit empty scalar.
   function After_Param_Sep_Props
     (P : in out Class; E : out Event) return Boolean;

   --  expects either a `,` or a `)`
   function After_Param (P : in out Class; E : out Event)
                         return Boolean;

   --  end an annotation
   function After_Annotation (P : in out Class; E : out Event)
                              return Boolean;
end Yaml.Parser;
