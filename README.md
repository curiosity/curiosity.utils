# curiosity.utils

This is for generic utility functions in use across Curiosity's clojure 
projects. It remixes its own functions along with those from prismatic/plumbing,
com.taeonsso/encore, and potemkin.

## Docs

Run `lein doc` for html api docs. See docstrings or `clojure.repl/doc` otherwise.

Semi-recent output from `lein doc` resides here: https://curiosityapp.github.io/curiosity.utils

## Installation

[![Clojars Project](http://clojars.org/curiosity.utils/latest-version.svg)](http://clojars.org/curiosity.utils)

## Usage

```clj
(require '[curiosity.utils :refer :all])
```

## Testing

Tests live under `test/` and are run via CircleCI. 

[![Latest Version](https://circleci.com/gh/curiosity/curiosity.utils.svg?style=svg&circle-token=6a84b5949665cc4ca73e868c41339be82e8e066b)](https://circleci.com/gh/curiosity/curiosity.utils)

## License

Copyright © 2015 Beacon Solutions, Inc

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
