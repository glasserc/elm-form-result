module Form.Result.AnyJust exposing (AnyJust, start, check, finish, map, andMap, andThen)

{-| A helper type to look for a Just in a pile of Maybes.


## Background

Let's imagine you have a FormError type like this:

    type alias FormError =
        { password : Maybe PasswordStrengthError
        , acceptedTerms : Maybe MustAcceptTermsError
        }

You might produce these values by validating a registration
form. You'd augment the form with any errors you produced, and as the
user updated the form, you would clear the error fields (until you
validated it again).

You have a submit button but you want to disable it if there's an
error that needs correcting, so you add a `disabled` attribute
according to the condition `Maybe.isJust formErrors`. But eventually
the user will clear the last error and you'll have a FormError full of
`Nothing`s. You need to convert that `Just` "empty" FormError into a
`Nothing` so that the submit button can be activated again.

You could write a function like this:

    flatten : FormErrors -> Maybe FormErrors
    flatten ({ password, acceptedTerms } as formErrors) =
        case ( password, acceptedTerms ) of
            ( Nothing, Nothing ) ->
                Nothing

            _ ->
                Just formErrors

But this function is brittle. If you add a field to the `FormErrors`
type, this function needs to be updated too, and the compiler won't
help you.

Instead, this module provides an `AnyJust` type that re-uses the
constructor to your `FormErrors` type to ensure that every field is
checked for presence. You could re-write the `flatten` function with
it as:

    flatten : FormErrors -> Maybe FormErrors
    flatten formErrors =
        AnyJust.start FormErorrs
            |> AnyJust.check formErrors.password
            |> AnyJust.check formErrors.acceptedTerms
            |> AnyJust.finish

As the name implies, if any field is `Just`, then at the end, you wind
up with a `Just FormErrors` (produced by re-constructing it with the
values given). If no `Just` was provided to `AnyJust.check`, then you
end up with `Nothing`. The reconstruction of the `FormErrors` is
slightly inefficient, but in exchage, we can leverage the compiler to
complain if we add another field.

@docs AnyJust, start, check, finish, map, andMap, andThen

-}

import Maybe.Extra as MaybeEx


{-| A utility type for checking for a `Just` in a pile of `Maybe`s.

It wraps some other type, typically a constructor, and as you feed it
arguments using `check`, it updates a `Bool` to see if it ever saw
a `Just`. At first, this `Bool` is `False`. It switches to `True`
if `check` is ever given a `Just`.

You can feel free to think of this type as a writer monad for a
Bool. You can also feel free not to think of it that way, if you
don't know what that means or find it confusing.

-}
type alias AnyJust a =
    { model : a
    , seenJust : Bool
    }


{-| Start an `AnyJust` that has never seen a `Just`.
-}
start : a -> AnyJust a
start model =
    { model = model
    , seenJust = False
    }


{-| Feed a `Maybe` to the underlying type, checking if it's a `Just`.
-}
check : Maybe a -> AnyJust (Maybe a -> b) -> AnyJust b
check nextField { model, seenJust } =
    { model = model nextField
    , seenJust = seenJust || MaybeEx.isJust nextField
    }


{-| Resolve the `AnyJust`, producing `Just a` if we ever saw a `Just`.
-}
finish : AnyJust a -> Maybe a
finish { model, seenJust } =
    if not seenJust then
        Nothing

    else
        Just model


{-| Functor `map`, just for completeness.

Be careful not to use this instead of `check` if you intend to check a
`Maybe`. If you do, the types will still work out, but the `finish`
function will not produce a `Just` at the end.

-}
map : (a -> b) -> AnyJust a -> AnyJust b
map f anyJust =
    { model = f anyJust.model
    , seenJust = anyJust.seenJust
    }


{-| Applicative `andMap`, just for completeness.

The resulting `AnyJust` will register as having seen a `Just` if either
input saw a `Just`.

Be careful not to use this instead of `check` if you intend to check a
`Maybe`. If you do, the types will still work out, but the `finish`
function will not produce a `Just` at the end.

-}
andMap : AnyJust a -> AnyJust (a -> b) -> AnyJust b
andMap anyJust anyF =
    { model = anyF.model anyJust.model
    , seenJust = anyF.seenJust || anyJust.seenJust
    }


{-| Monadic `andThen`, just for completeness.

The resulting `AnyJust` will register as having seen a `Just` if the
original `AnyJust` saw a `Just` or if the output `AnyJust` saw a
`Just`.

Be careful not to use this instead of `check` if you intend to check a
`Maybe`. If you do, the types will still work out, but the `finish`
function will not produce a `Just` at the end.

-}
andThen : (a -> AnyJust b) -> AnyJust a -> AnyJust b
andThen f anyJust =
    let
        anyOut =
            f anyJust.model
    in
    { model = anyOut.model
    , seenJust = anyOut.seenJust || anyJust.seenJust
    }
