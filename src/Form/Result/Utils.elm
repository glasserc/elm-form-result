module Form.Result.Utils exposing (ifNothing, nothingAs)

{-| Some basic utilities that don't belong anywhere else.

@docs ifNothing, nothingAs

-}


{-| Look for `Nothing`, using some other value to show you found it.

This comes in handy when you want to e.g. react to an absent field by
raising an error, but don't need to do anything when the field is
present.

-}
ifNothing : Maybe a -> b -> Maybe b
ifNothing val alt =
    case val of
        Nothing ->
            Just alt

        Just _ ->
            Nothing


{-| Same as `ifNothing`, but with the argument order reversed.
-}
nothingAs : b -> Maybe a -> Maybe b
nothingAs alt val =
    ifNothing val alt
