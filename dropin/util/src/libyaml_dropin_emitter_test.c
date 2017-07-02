#include "yaml.h"
#include <stdio.h>

int main(int argc, char* argv[]) {
  yaml_emitter_t emitter;
  yaml_emitter_initialize (&emitter);
  yaml_event_t event;
  unsigned char buffer[8192];
  size_t written;
  yaml_emitter_set_output_string (&emitter, buffer, 8192, &written);

  yaml_stream_start_event_initialize (&event, YAML_ANY_ENCODING);
  yaml_emitter_emit (&emitter, &event);

  yaml_document_start_event_initialize (&event, NULL, NULL, NULL, 0);
  yaml_emitter_emit (&emitter, &event);

  yaml_mapping_start_event_initialize (&event, (unsigned char*) "",
				       (unsigned char*) "tags:yaml.org,2002:map", 1,
				       YAML_BLOCK_MAPPING_STYLE);
  yaml_emitter_emit (&emitter, &event);

  yaml_scalar_event_initialize (&event, (unsigned char*)"", (unsigned char*)"",
				(unsigned char*)"a", 1, 1, 1, YAML_ANY_SCALAR_STYLE);
  yaml_emitter_emit (&emitter, &event);

  yaml_mapping_start_event_initialize (&event, (unsigned char*)"",
				       (unsigned char*)"", 0,
				       YAML_FLOW_MAPPING_STYLE);
  yaml_emitter_emit (&emitter, &event);

  yaml_scalar_event_initialize (&event, (unsigned char*)"foo", (unsigned char*)"",
				(unsigned char*)"derp", 1, 1, 1, YAML_ANY_SCALAR_STYLE);
  yaml_emitter_emit (&emitter, &event);

  yaml_sequence_start_event_initialize
    (&event, (unsigned char*)"", (unsigned char*)"", 0, YAML_FLOW_SEQUENCE_STYLE);
  yaml_emitter_emit (&emitter, &event);

  yaml_sequence_end_event_initialize (&event);
  yaml_emitter_emit (&emitter, &event);

  yaml_mapping_end_event_initialize (&event);
  yaml_emitter_emit (&emitter, &event);

  yaml_scalar_event_initialize (&event, (unsigned char*)"", (unsigned char*)"",
				(unsigned char*)"herp", 1, 1, 1,
				YAML_DOUBLE_QUOTED_SCALAR_STYLE);
  yaml_emitter_emit (&emitter, &event);

  yaml_alias_event_initialize (&event, (unsigned char*)"foo");
  yaml_emitter_emit (&emitter, &event);

  yaml_mapping_end_event_initialize (&event);
  yaml_emitter_emit (&emitter, &event);

  yaml_document_end_event_initialize (&event, 0);
  yaml_emitter_emit (&emitter, &event);

  yaml_stream_end_event_initialize (&event);
  yaml_emitter_emit (&emitter, &event);

  yaml_emitter_delete (&emitter);

  buffer[written] = 0;
  puts((char*)buffer);
}
