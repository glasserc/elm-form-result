module Form.Result exposing
    ( FormResult, start, validated, toResult
    , maybeValid, maybeErr
    , checkErr, ifMissing
    , unconditional, unconditionalErr
    )

{-| A type for validating a form, collecting errors as we go.

Imagine you have a type that represents the data a user has currently
entered into a form. For example, maybe it looks like this:

    type alias FormData =
        { username : String
        , password : String
        , confirmPassword : String
        }

You want to validate it into a user object:

    type alias User =
        { username : String
        , password : String
        }

When we build up our user type, even one bad field is enough to fail
the validation. But when we display the invalid form, we want to show
the problems for every single field, not just the first bad one. So
when validation doesn't succeed, you want to produce a `FormErrors`:

    type UsernameError
        = UsernameMissing

    type PasswordError
        = PasswordNotStrongEnough
        | PasswordsDontMatch

    type alias FormErrors =
        { username : Maybe UsernameError
        , password : Maybe PasswordError
        , confirmPassword : Maybe PasswordError
        }

We want to stop building the User as soon as we get one failed field,
so you could try using `Maybe.Extra.andMap`:

    validateUser : FormData -> Maybe User
    validateUser state =
        Just (\username password _ -> User username password)
            |> MaybeEx.andMap (checkStringEmpty state.username)
            |> MaybeEx.andMap (checkPasswordStrength state.password)
            |> MaybeEx.andMap (checkMatch state.password state.confirmPassword)

But then what if the user was Nothing? You have to re-use all those
field checks and even invert them to produce errors instead of
successes so that you can use them in the `FormErrors` type.

`FormResult` solves this problem by building up both the error type
and the validated "output" type at the same time. You feed it
`Result`s and it feeds `Err`s to the error type and `Ok`s to the
"output" type. After all the fields have been provided, you can
convert the whole thing into a `Result`, which will be `Ok` if all the
fields were `Ok`, or `Err` otherwise.

    validateUsername : String -> Either UsernameError String
    validateUsername s =
        if s == "" then Err UsernameMissing else Ok s

    validatePassword : String -> Either PasswordError String
    validatePassword s = ...

    validateMatch : String -> String -> Either PasswordError String
    validateMatch password confirmPassword = ...

    validateUser : FormData -> Either FormErrors User
    validateUser state =
        Form.Result.start FormErrors User
            |> Form.Result.validate (validateUsername state.username)
            |> Form.Result.validate (validatePassword state.password)
            |> Form.Result.checkErr (validateMatch state.password state.confirmPassword)
            |> Form.Result.toResult

Note that although `state.confirmPassword` does not contribute
anything to our output type, we still need to check it for errors, and
any errors in it should cause the validation to fail.

A fully-functioning demo can be found in the
[examples](https://github.com/glasserc/elm-form-result/tree/master/examples)
directory in [this project's
repository](https://github.com/glasserc/elm-form-result).


## Hints

  - `FormResult` is easiest to use if the order of fields in your error
    type corresponds to the order of fields in your output type. (Your
    form state type and generated HTML can be in any order you like, of
    course.) If you have to feed fields to your output type and your
    error type in a different order, you can do that using
    `maybeValid` and `maybeErr`, but it's easy to get confused.

  - If you find yourself with a `FormResult` that has a lot of fields
    and you're losing track of what field belongs to what output type,
    you might consider splitting up your validation function into
    smaller validation functions, producing smaller types. For an
    example of this, check `validateFraction` in [the
    example](https://github.com/glasserc/elm-form-result/blob/master/examples/src/Main.elm).

@docs FormResult, start, validated, toResult
@docs maybeValid, maybeErr
@docs checkErr, ifMissing
@docs unconditional, unconditionalErr

-}

import Form.Result.Utils exposing (errToMaybe)
import Maybe.Extra as MaybeEx


{-| An "in progress" form validation.
The first type variable `err` is the form errors type.
The second type variable `res` is the "real model" type, which you
hope to get if validation succeeds.
-}
type alias FormResult err res =
    { errorType : err
    , realModel : Maybe res
    }


{-| Create a form validation.
-}
start : err -> res -> FormResult err res
start err res =
    { errorType = err
    , realModel = Just res
    }


{-| Add a field to an "in progress" form validation, with `Err
something` indicating that validation has failed.
-}
validated : Result a b -> FormResult (Maybe a -> err) (b -> res) -> FormResult err res
validated fieldR formResult =
    { errorType = formResult.errorType (errToMaybe fieldR)
    , realModel = formResult.realModel |> MaybeEx.andMap (Result.toMaybe fieldR)
    }


{-| Add something to just the error side of an "in progress" form
validation, with `Just err` meaning that validation has failed.

This can be useful if multiple fields in your form type correspond to
a single field in your output type. In this case, you'll probably have
several fields in your form error type that don't correspond to
anything in your output type, so you'll want to feed a possible error
in to each without touching the output type.

-}
maybeErr : Maybe errField -> FormResult (Maybe errField -> err) res -> FormResult err res
maybeErr err formResult =
    { errorType = formResult.errorType err
    , realModel =
        case err of
            Just _ ->
                Nothing

            Nothing ->
                formResult.realModel
    }


{-| Add a possible output field to an "in progress" form validation,
with `Nothing` indicating that validation has failed.

This can be useful if multiple fields in your form type correspond to
a single field in your output type. In this case, a field in your
output type might not be present, but without an error in the very
next form error field. In that case, you can use this function to
incorporate a `Maybe field`, and add `Maybe error`s using
`maybeErr`.

-}
maybeValid : Maybe resField -> FormResult err (resField -> res) -> FormResult err res
maybeValid fieldM formResult =
    { errorType = formResult.errorType
    , realModel = MaybeEx.andMap fieldM formResult.realModel
    }


{-| A shortcut for calling `maybeErr` with a `Result` instead of a
`Maybe`.

If the field is `Err`, it indicates that validation failed.
If the field is `Ok whatever`, discard the `whatever` and call the
error type with `Nothing`.

This is useful when you have a validation function that produces a
`Result err something` but you don't actually care about the `something`.

-}
checkErr : Result errField a -> FormResult (Maybe errField -> err) res -> FormResult err res
checkErr =
    maybeErr << errToMaybe


{-| A shortcut for calling `validated` with a `Maybe` instead of a
`Result`.

If the field is `Nothing`, it indicates that the validation failed,
and the given error is what is used to indicate the error.
If the field is `Just something`, use the `something` as the output
and call the error type with `Nothing`.

This is useful e.g. when handling select fields, where the only thing
you want to validate is that the user actually selected something.

-}
ifMissing : Maybe resField -> errField -> FormResult (Maybe errField -> err) (resField -> res) -> FormResult err res
ifMissing fieldM err =
    validated (Result.fromMaybe err fieldM)


{-| Add a field to the output type of an "in progress" form
validation, unconditionally.

You might use this if there are fields in your output type that are
not dependent on user input.

-}
unconditional : resField -> FormResult err (resField -> res) -> FormResult err res
unconditional field formResult =
    { errorType = formResult.errorType
    , realModel = MaybeEx.andMap (Just field) formResult.realModel
    }


{-| Add something to just the error side of an "in progress" form
validation, unconditionally.

This can be useful if your error type has fields that aren't Maybe, or
when `Just err` in an error field doesn't necessarily mean that the
validation failed.

-}
unconditionalErr : errField -> FormResult (errField -> err) res -> FormResult err res
unconditionalErr err formResult =
    { errorType = formResult.errorType err
    , realModel = formResult.realModel
    }


{-| Convert a FormResult to a Result.
-}
toResult : FormResult err res -> Result err res
toResult formResult =
    Result.fromMaybe formResult.errorType formResult.realModel
