# elm-review-html-lazy

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules detect incorrect usage of [`Html.Lazy`](https://package.elm-lang.org/packages/elm/html/latest/Html.Lazy) and [`Html.Styled.Lazy`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html.Styled.Lazy) .


## Provided rules

- [`NoEqualityBreakingLazyArgs`](https://package.elm-lang.org/packages/noredink/elm-review-html-lazy/1.0.0/NoEqualityBreakingLazyArgs) - Reports arguments to `lazy` calls that we can definitively prove will always break the reference equality check.


## Configuration

```elm
module ReviewConfig exposing (config)

import NoEqualityBreakingLazyArgs
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ NoEqualityBreakingLazyArgs.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template noredink/elm-review-html-lazy/example
```
