# elm-review-string-template [![Build Status](https://travis-ci.org/emmabastas/elm-review-string-template.svg?branch=master)](https://travis-ci.org/emmabastas/elm-review-string-template)

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to REPLACEME.


## Provided rules

- [`NoBadTemplateInject`](https://package.elm-lang.org/packages/emmabastas/elm-review-string-template/1.0.0/NoBadTemplateInject) - Reports REPLACEME.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoBadTemplateInject
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoBadTemplateInject.rule
    ]
```
