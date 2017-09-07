# AdaYaml Changelog

## v0.2.0

 * passes all tests of the yaml-test-suite
 * outsourced Text package to ParserText project
 * fixed a bug in Text package that led to an endless loop when allocating
 * restructured Reference types to not have a discriminant so that they can
   be used in records. Access to the underlying type is now provided by a
   Value function.
 * Fixed several assertion failures that have gone unnoticed because the project
   was compiled without -gnata

## v0.1.0

Initial release.