#include "yaml.h"

#include<stdio.h>

extern void adainit (void);
extern void adafinal (void);

void put_properties(yaml_char_t* anchor, yaml_char_t* tag) {
  if (*anchor != 0) {
    printf(" &%s", anchor);
  }
  if (*tag != 0) {
    printf(" <%s>", tag);
  }
}

int main(int argc, char* argv[]) {
  yaml_parser_t parser;
  yaml_parser_initialize(&parser);
  yaml_parser_set_input_string
    (&parser, (unsigned char*) "a: !!map {\n  ? b : c\n }\n!foo bar: *foo", 40);
  yaml_event_t event;
  for(;;) {
    yaml_parser_parse(&parser, &event);
    switch(event.type) {
      case YAML_STREAM_START_EVENT:
	puts("+STR"); break;
      case YAML_STREAM_END_EVENT:
	puts("-STR"); break;
      case YAML_DOCUMENT_START_EVENT:
	printf("+DOC%s\n", event.data.document_start.implicit ? "" : " ---");
	break;
      case YAML_DOCUMENT_END_EVENT:
	printf("-DOC%s\n", event.data.document_end.implicit ? "" : " ...");
	break;
      case YAML_MAPPING_START_EVENT:
	printf("+MAP");
	put_properties(event.data.mapping_start.anchor, event.data.mapping_start.tag);
	puts("");
	break;
      case YAML_MAPPING_END_EVENT:
	puts("-MAP");
	break;
      case YAML_SEQUENCE_START_EVENT:
	printf("+SEQ");
	put_properties(event.data.sequence_start.anchor, event.data.sequence_start.tag);
	puts("");
	break;
      case YAML_SEQUENCE_END_EVENT:
	puts("-SEQ");
	break;
      case YAML_ALIAS_EVENT:
	printf("*ALI &%s\n", event.data.alias.anchor);
	break;
      case YAML_SCALAR_EVENT:
	printf("=VAL");
	put_properties(event.data.scalar.anchor, event.data.scalar.tag);
	switch(event.data.scalar.style) {
	  case YAML_ANY_SCALAR_STYLE:
	  case YAML_PLAIN_SCALAR_STYLE:
	    printf(" :");
	    break;
	  case YAML_DOUBLE_QUOTED_SCALAR_STYLE:
	    printf(" \"");
	    break;
	  case YAML_SINGLE_QUOTED_SCALAR_STYLE:
	    printf(" \'");
	    break;
	  case YAML_LITERAL_SCALAR_STYLE:
	    printf(" |");
	    break;
	  case YAML_FOLDED_SCALAR_STYLE:
	    printf(" >");
	    break;
	}
	puts((char*)event.data.scalar.value);
	break;
      case YAML_NO_EVENT:
	puts(":NIL");
	break;
    }
    fflush(stdout);
    if (event.type == YAML_STREAM_END_EVENT) { break; }
    yaml_event_delete(&event);
  }
  yaml_event_delete(&event);
  yaml_parser_delete(&parser);
}
