#include "yaml.h"
#include <stdio.h>

void put_properties(yaml_char_t* anchor, yaml_char_t* tag) {
  if (anchor) {
    printf(" &%s", anchor);
  }
  if (tag) {
     printf(" <%s>", tag);
  }
}

int main(int argc, char* argv[]) {
  yaml_parser_t parser;
  yaml_parser_initialize(&parser);
  FILE* file = NULL;
  if (argc == 1) {
    yaml_parser_set_input_file(&parser, stdin);
  } else {
    file = fopen(argv[1], "r");
    if (!file) {
      printf("Cannot open file: %s\n", argv[1]);
      exit(EXIT_FAILURE);
    }
    yaml_parser_set_input_file(&parser, file);
  }
  yaml_event_t event;
  for(;;) {
    if (!yaml_parser_parse(&parser, &event)) {
       printf("Parser Error:\n  %s\n", parser.problem);
       exit(EXIT_FAILURE);
    }
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
      case YAML_ANNOTATION_START_EVENT:
	printf("+ANN");
	put_properties(event.data.annotation_start.anchor,
		       event.data.annotation_start.tag);
	printf(" @%s\n", event.data.annotation_start.name);
	break;
      case YAML_ANNOTATION_END_EVENT:
	puts("-ANN");
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
  if (file) {
    fclose(file);
  }
}
