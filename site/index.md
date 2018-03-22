---
layout: page
title: About
permalink: /
weight: 1
---

**AdaYaml** is an experimental YAML implementation in Ada 2012. *Experimental*
means that it does not conform to any released YAML specification. Instead, it
is a testbed for the proposed YAML 1.3 standard. This [list of RFCs][1] gives an
overview of the differences between YAML 1.2 and upcoming YAML 1.3. Since
YAML 1.3 is not finished, everything is due to change.

However, most of the changes for YAML 1.3 are cosmetic and have been carefully
edited to not break common YAML 1.2 documents. So if you are searching for a
YAML 1.2 implementation, AdaYaml may still be a viable option for you which only
breaks in rather obscure edge cases.

Currently, AdaYaml implements *parsing* and *presenting* YAML data, i.e.
you can use it to transform a character stream (a file, a string, …) into a
sequence of YAML events and vice versa. There also is currently a DOM API which
allows you to transform a YAML input to a DOM structure and vice versa.
AdaYaml currently does not support serialising arbitrary Ada data structures.

## Status

The parser is pretty well-tested with the external [yaml-test-suite][2] which
strives to be part of the upcoming YAML 1.3 standard. Other parts of the API
have been tested less extensively.

## Project Goals

The goals of this project are:

 * To evaluate YAML 1.3 RFCs by implementing them.
 * To evolve into the YAML 1.3 reference implementation.
 * To offer a drop-in alternative to libyaml by providing the C-level interface
   of `yaml.h`.

AdaYaml is **not** designed to be used under hard memory constraints.

## Releases

Releases are available as [tags][3] of the [GitHub repository][4].

## License

AdaYaml is available under the terms of the [MIT license][5].

 [1]: https://github.com/yaml/summit.yaml.io/wiki/YAML-RFC-Index
 [2]: https://github.com/yaml/yaml-test-suite
 [3]: https://github.com/yaml/AdaYaml/tags
 [4]: https://github.com/yaml/AdaYaml
 [5]: https://opensource.org/licenses/MIT