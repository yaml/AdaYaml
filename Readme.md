# AdaYaml - YAML 1.3 implementation in Ada

This is an implementation of the upcoming YAML 1.3 standard. Since the standard
is not completed yet, some features are due to change.

You can use it as YAML 1.2 implementation, it will parse almost all valid
YAML 1.2 documents since proposed changes to 1.3 are minimal and only cover edge
cases.

## Status

Current YAML test suite status:

```
Total Tests Run:   249
Successful Tests:  244
Failed Assertions: 2
Unexpected Errors: 3
```

The parser is thoroughly tested and only fails test cases that are expected not
to pass in YAML 1.3. Lionel Matias has [fuzzy-tested AdaYaml][3], his findings
have been adressed. Benchmarks of AdaYaml show that it currently uses about
twice the time libyaml uses for parsing large YAML documents.

The parts of AdaYaml which have not undergone extensive testing are the DOM API
and the Presenter.

## Installation

You can install AdaYaml easily by executing

    gprbuild -p -XMode=release yaml.gpr
    gprinstall -XMode=release yaml.gpr

Afterwards, you can `with "yaml";` in your projects.

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
 - [x] Do some benchmarks
 - [ ] Web demo
 - [ ] Implement UTF-16 and UTF-32 encodings
 - [ ] Make AdaYaml a drop-in replacement for libyaml (provide the C interface)
 - [ ] As proof-of-concept, implement the [Transformations extension][2]
 - [x] Maybe provide a DOM API

## Hacking

You can build the tests with:

    make test

This builds the unit tests. The executables will be located in the
`test/bin` directory (be sure that the root folder is the working directory when
executing).

If you have [GNAT GPL](http://libre.adacore.com/download/configurations) and
[GNATcoverage](https://github.com/AdaCore/gnatcoverage), you can also compute
code coverage from the testsuite:

    make test-coverage-run

The following command builds a small utility that reads a YAML files and outputs
a stream of events to the command line:

    make utils

It is located in `util/bin` afterwards. The tool reads either the file
specified as first command line parameter, or, in absense of that parameter,
from stdin.

To edit the code, you can use GNAT Programming Studio to open the `*.gpr` files.

## License

[MIT](copying.txt)

 [1]: https://github.com/yaml/summit.yaml.io/wiki/YAML-RFC-Index
 [2]: https://github.com/yaml/yaml-spec/wiki/The-Transformations-Extension
 [3]: https://blog.adacore.com/running-american-fuzzy-lop-on-your-ada-code
