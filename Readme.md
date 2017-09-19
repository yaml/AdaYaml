# AdaYaml - YAML 1.3 implementation in Ada

This is an implementation of the upcoming YAML 1.3 standard. This library is
currently under construction. Be wary of errors, I have not even proven it
correct.

## Status

Current YAML test suite status:

```
Total Tests Run:   224
Successful Tests:  224
Failed Assertions: 0
Unexpected Errors: 0
```

## YAML 1.3 Features

Status of YAML 1.3 features, taken from [this list][1]:

| Number  | Desciption                                       | Status
|---------|--------------------------------------------------|----------
| RFC-001 | Remove the 1024 character limit                  | Implemented
| RFC-002 | Limit content allowed after the '---' header     | Implemented
| RFC-003 | Characters that can appear in anchor             | Implemented
| RFC-004 | Block sequence entries must use space after dash | Implemented
| RFC-005 | Restrict default implicit types to JSON semantics| Not applicable
| RFC-006 | Disallow carriage return as a break              | Implemented
| RFC-007 | Disallow flow collections as implicit keys       | Implemented
| RFC-008 | Annotations                                      | Can be parsed
| RFC-009 | Allow unresolvable aliases                       | Implemented
| RFC-010 | Fixed position of properties and block scalars   | Implemented
| RFC-011 | Indentation of block scalars                     | Implemented
| RFC-012 | Remove wiki-like syntax from folded block scalars| Implemented
| RFC-013 | *retracted*                                      | Not implemented
| RFC-014 | Anchors always come before tags (when both)      | Not implemented

## General Roadmap

 - [x] Have a basic working lexer
 - [x] Have a basic working parser
 - [x] Have a basic working representer
 - [x] Test against the YAML test suite
 - [ ] Implement fancy error reporting (starting line of error with marker)
 - [ ] Do some benchmarks
 - [ ] Web demo
 - [ ] Implement UTF-16 and UTF-32 encodings
 - [ ] Make AdaYaml a drop-in replacement for libyaml (provide the C interface)
 - [ ] As proof-of-concept, implement the [Transformations extension][2]
 - [ ] Maybe provide a DOM API

## Hacking

These commands may be useful for toying around:

    make adayaml

This builds the library. Not really useful right now.

    make test

This builds the unit tests. The executables will be located in the
`test/bin` directory (be sure that the root folder is the working directory when
executing).

    make utils

This builds a small utility that reads a YAML files and outputs a stream of
events to the command line. It is located in `util/bin` afterwards. The tool
reads either the file specified as first command line parameter, or, in absense
of that parameter, from stdin.

To edit the code, you can use GNAT Programming Studio to open the `*.gpr` files.

## License

[MIT](copying.txt)

 [1]: https://github.com/yaml/summit.yaml.io/wiki/YAML-RFC-Index
 [2]: https://github.com/yaml/summit.yaml.io/wiki/The-Transformations-Extension
