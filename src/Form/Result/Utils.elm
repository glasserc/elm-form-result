module Form.Result.Utils exposing (errToMaybe, ifNothing)

{-| Some basic utilities that don't belong anywhere else.

@docs errToMaybe, ifNothing

-}


{-| Extract the `Err` from a `Result`, if present.
-}



-- FIXME: this probably belongs in Result or Result.Extra. It's the
-- same as the `error` function from
-- https://github.com/elm-community/result-extra/pull/19, but it's not
-- clear what the fate of that PR is.


errToMaybe : Result err a -> Maybe err
errToMaybe r =
    case r of
        Ok _ ->
            Nothing

        Err err ->
            Just err


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
