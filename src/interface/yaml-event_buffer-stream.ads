--  part of AdaYaml, (c) 2017 Felix Krause
--  released under the terms of the MIT license, see the file "copying.txt"

with Yaml.Stream_Concept;

package Yaml.Event_Buffer.Stream is new Stream_Concept (Instance, Next);
