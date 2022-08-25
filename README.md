# elm-review-html-lazy

Provides [`elm-review`](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) rules to detect incorrect usage of:
- [`Html.Lazy`](https://package.elm-lang.org/packages/elm/html/latest/Html.Lazy)
- [`Html.Styled.Lazy`](https://package.elm-lang.org/packages/rtfeldman/elm-css/latest/Html.Styled.Lazy)
- [`Element.Lazy`](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/Element-Lazy)
- [`Element.WithContext.Lazy`](https://package.elm-lang.org/packages/miniBill/elm-ui-with-context/latest/Element-WithContext-Lazy).


## Provided rules

- [`UseMemoizedLazyLambda`](https://package.elm-lang.org/packages/noredink/elm-review-html-lazy/1.0.0/UseMemoizedLazyLambda) - Require calling `lazy` at the top level of a point-free function with a lambda expression as the first argument so that the lambda expression is always memoized.

## Configuration

```elm
module ReviewConfig exposing (config)

import UseMemoizedLazyLambda
import Review.Rule exposing (Rule)

config : List Rule
config =
    [ UseMemoizedLazyLambda.rule
    ]
```


## Try it out

You can try the example configuration above out by running the following command:

```bash
elm-review --template noredink/elm-review-html-lazy/example
```
