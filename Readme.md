# AdaYaml - YAML 1.3 implementation in Ada

This is an implementation of the upcoming YAML 1.3 standard. This library is
currently under construction. Be vary of errors, I have not even proven it
correct.

## YAML 1.3 Features

Status of YAML 1.3 features, taken from [this list][1]:

| Number  | Desciption                                       | Status
|---------|--------------------------------------------------|----------
| RFC-001 | Remove the 1024 character limit                  | Implemented
| RFC-002 | Limit content allowed after the '---' header     | Needs testing
| RFC-003 | Characters that can appear in anchor             | Implemented
| RFC-004 | Block sequence entries must use space after dash | Implemented
| RFC-005 | Restrict default implicit types to JSON semantics| Not applicable
| RFC-006 | Disallow carriage return as a break              | Not implemented
| RFC-007 | Disallow flow collections as implicit keys       | Implemented
| RFC-008 | Annotations                                      | Can be parsed
| RFC-009 | Allow unresolvable aliases                       | Implemented
| RFC-010 | Fixed position of properties and block scalars   | Not implemented
| RFC-011 | Indentation of block scalars                     | Implemented
| RFC-012 | Remove wiki-like syntax from folded block scalars| Implemented
| RFC-013 | *retracted*                                      | Not implemented
| RFC-014 | Anchors always come before tags (when both)      | Not implemented

## General Roadmap

 - [x] Have a basic working lexer
 - [x] Have a basic working parser
 - [ ] Have a basic working representer
 - [ ] Test against the YAML test suite
 - [ ] Implement fancy error reporting (starting line of error with marker)
 - [ ] Do some benchmarks
 - [ ] Web demo
 - [ ] Make AdaYaml a drop-in replacement for libyaml (provide the C interface)
 - [ ] As proof-of-concept, implement the [Transformations extension][2]

## License

MIT. I was too lazy to add proper licensing comments and stuff yet.

 [1]: https://github.com/yaml/summit.yaml.io/wiki/YAML-RFC-Index
 [2]: https://github.com/yaml/summit.yaml.io/wiki/The-Transformations-Extension