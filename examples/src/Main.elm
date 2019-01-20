module Main exposing (main)

{-| A simple application demonstrating how to use Form.Result.

This program shows a hypothetical "user registration" form, where the
user is expected to enter:

  - a username, which is required, and cannot be "admin" or "root"
    (which are "in use")

  - a password, which must contain at least one number, one letter, and
    one punctuation mark

  - a "confirm password" field, which must be identical to the password
    field

  - a "favorite fraction", where the user must enter a numerator and
    denominator, such as 1/5. The denominator must be nonzero and the
    fraction must be between 0 and 1 inclusive.

  - a "terms and conditions" checkbox, which must be checked

We also track the number of times the user has clicked "Submit". If
you had to try too many times, maybe we don't want you as a user!

Styling is left as an exercise for the reader.

-}

import Browser
import Debug
import Form.Result exposing (checkErr, maybeErr, maybeValid, unconditional, unconditionalErr, validated)
import Form.Result.AnyJust as AnyJust
import Form.Result.Utils exposing (nothingAs)
import Html exposing (Html, button, div, form, h1, input, label, li, span, text, ul)
import Html.Attributes as Attributes
import Html.Events as Events
import Maybe.Extra as MaybeEx


type alias Fraction =
    ( Int, Int )


type alias User =
    { username : String
    , password : String
    , fraction : Fraction

    -- Of course, a real "user" type wouldn't track how many attempts
    -- it took for the person at the keyboard to get a correct
    -- document, but I'm including this as a field to demonstrate `unconditional`.
    , submitAttempts : Int
    }


type alias FormState =
    { liveValidate : Bool
    , username : String
    , password : String
    , confirmPassword : String
    , favoriteNumerator : String
    , favoriteDenominator : String
    , acceptTerms : Bool
    , submitAttempts : Int
    , errors : Maybe FormErrors
    }


type UsernameError
    = UsernameMissing
    | UsernameInUse String


{-| Many things can go wrong with the password.

I chose to represent this as a record because each could be true
sort-of independently of the others, but each can only be true at most
once and order does not matter. Depending on what kinds of things can
go wrong with your field, you might choose to have an error type that
was a List of some enumerated type. As long as you can build a Result
of it, Form.Result doesn't really care.

-}
type alias PasswordErrors =
    { passwordMissing : Bool
    , passwordNoLetter : Bool
    , passwordNoNumber : Bool
    , passwordNoSymbol : Bool
    }


{-| The "Confirm Password" field only validates that the field is
present and matches the password field. The password field is
responsible for all other validations.

Note that the two passwords not matching could be conceivably a bug
with either field. In the [README](https://github.com/glasserc/elm-form-result/blob/master/README.md),
I describe this kind of error as a "bad combination" error, and I
suggest multiple ways to handle it. This is the "arbitrarily add it to
one field" approach. In theory, editing the password field could
resolve this problem, but to make that work correctly, I would have to
add special-case code to the UpdatePassword handler, and I
didn't. This means if you change the first password field to match the
confirm, we don't clear the error.

-}
type ConfirmPasswordError
    = ConfirmPasswordMissing
    | ConfirmPasswordDoesntMatch


{-| We also have a "bad combination" situation with the fraction
fields, since we want to ensure n <= d, but when it isn't, it could be
because n is too big, or because d is too small. This is the
"introduce a field just for the combination" approach. We clear this
field's error when either the numerator or the denominator changes,
but we have to be careful to make sure to display this error, which is
easier to forget since it isn't attached to any specific output field.
-}
type FractionCombinationError
    = FractionImproper


type NumeratorError
    = NumeratorMissing
    | NumeratorNotNumeric
    | NumeratorNegative



-- This shares a lot with the NumeratorError and in a real application
-- you might want to refactor the two types to actually share constructors.


type DenominatorError
    = DenominatorMissing
    | DenominatorNotNumeric
    | DenominatorNegative
    | DenominatorZero


type alias FractionErrors =
    { numerator : Maybe NumeratorError
    , denominator : Maybe DenominatorError
    , combination : Maybe FractionCombinationError
    }


type AcceptTermsError
    = TermsNotAccepted


type alias FormErrors =
    { username : Maybe UsernameError
    , password : Maybe PasswordErrors
    , confirmPassword : Maybe ConfirmPasswordError
    , fractionError : Maybe FractionErrors
    , acceptTerms : Maybe AcceptTermsError
    }


type alias State =
    { formState : FormState
    , lastSuccess : Maybe User
    }


initialFormState : FormState
initialFormState =
    { liveValidate = False
    , username = ""
    , password = ""
    , confirmPassword = ""
    , favoriteNumerator = ""
    , favoriteDenominator = ""
    , acceptTerms = False
    , submitAttempts = 0
    , errors = Nothing
    }


initialState : State
initialState =
    { formState = initialFormState
    , lastSuccess = Nothing
    }


main =
    Browser.sandbox { init = initialState, update = update, view = view }


type Msg
    = UpdateLiveValidate Bool
    | UpdateUsername String
    | UpdatePassword String
    | UpdateConfirmPassword String
    | UpdateNumerator String
    | UpdateDenominator String
    | UpdateAcceptTerms Bool
    | SubmitClicked
    | ResetClicked


update : Msg -> State -> State
update msg model =
    let
        afterMsg =
            { model | formState = applyMsg msg model.formState }

        formState =
            afterMsg.formState
    in
    if msg == SubmitClicked then
        case validate afterMsg.formState of
            Err errors ->
                { afterMsg
                    | formState =
                        { formState | errors = Just errors }
                }

            Ok user ->
                { afterMsg | lastSuccess = Just user }

    else if afterMsg.formState.liveValidate then
        -- If live validation is turned on, then we validate the form
        -- anew on every action.
        -- This isn't necessarily how a real application would
        -- work. You might instead want to do validation when cursor
        -- focus left a field, and only on the fields that are
        -- related to the field which was changed, but doing that
        -- doesn't really demonstrate anything interesting about the
        -- Form.Result API.
        case validate afterMsg.formState of
            Err errors ->
                { afterMsg
                    | formState =
                        { formState
                            | errors = Just errors
                        }
                }

            Ok _ ->
                { afterMsg
                    | formState =
                        { formState | errors = Nothing }
                }

    else
        { afterMsg | formState = { formState | errors = Maybe.andThen flattenErrors afterMsg.formState.errors } }


validate : FormState -> Result FormErrors User
validate state =
    let
        checkedTerms =
            case state.acceptTerms of
                True ->
                    Nothing

                False ->
                    Just TermsNotAccepted
    in
    Form.Result.start FormErrors User
        |> validated (validateUsername state.username)
        |> validated (validatePassword state)
        |> checkErr (validateConfirmPassword state)
        |> validated (validateFraction state)
        |> maybeErr checkedTerms
        |> unconditional state.submitAttempts
        |> Form.Result.toResult


validateUsername : String -> Result UsernameError String
validateUsername s =
    case s of
        "" ->
            Err UsernameMissing

        "admin" ->
            Err (UsernameInUse s)

        "root" ->
            Err (UsernameInUse s)

        _ ->
            Ok s


validatePassword : FormState -> Result PasswordErrors String
validatePassword state =
    let
        presentPassword =
            case state.password of
                "" ->
                    Nothing

                _ ->
                    Just state.password

        findAny p l =
            case List.head (List.filter p l) of
                Nothing ->
                    False

                Just _ ->
                    True

        isSymbol c =
            -- Not available in Char, so just hack something up.
            -- A real implementation would probably check the Unicode
            -- char type.
            not (Char.isAlphaNum c)

        -- Define a little utility to check for properties in the
        -- password.  If the password is missing, then return True so
        -- we only show the "password missing" error.
        passwordHas p =
            MaybeEx.unwrap True (String.toList >> List.any p) presentPassword

        hasLetter =
            passwordHas Char.isAlpha

        hasDigit =
            passwordHas Char.isDigit

        hasSymbol =
            passwordHas isSymbol

        validPassword =
            presentPassword
                |> Maybe.andThen
                    (\s ->
                        if hasLetter && hasDigit && hasSymbol then
                            Just s

                        else
                            Nothing
                    )
    in
    Form.Result.start PasswordErrors identity
        |> unconditionalErr (MaybeEx.isNothing presentPassword)
        |> unconditionalErr (not hasLetter)
        |> unconditionalErr (not hasDigit)
        |> unconditionalErr (not hasSymbol)
        |> maybeValid validPassword
        |> Form.Result.toResult


{-| Verify that the confirmPassword is present and matches the
password.

Because `()` doesn't contain information, we could have equivalently
written this as returning `Maybe ConfirmPasswordError`, with `Nothing`
representing "no problems", but I find it a little counterintuitive to
use `Nothing` to represent a valid outcome. Instead we use a `Result`
because `Ok` is clearly positive. Also, this lets us demonstrate
`Form.Result.checkErr`.

-}
validateConfirmPassword : FormState -> Result ConfirmPasswordError ()
validateConfirmPassword state =
    case state.confirmPassword of
        "" ->
            Err ConfirmPasswordMissing

        s ->
            if s == state.password then
                Ok ()

            else
                Err ConfirmPasswordDoesntMatch


validateFraction : FormState -> Result FractionErrors Fraction
validateFraction state =
    let
        tryParseInt missing notNum negative field =
            case field of
                "" ->
                    Err missing

                s ->
                    case String.toInt s of
                        Nothing ->
                            Err notNum

                        Just i ->
                            if i < 0 then
                                Err negative

                            else
                                Ok i

        parsedNumerator =
            tryParseInt NumeratorMissing NumeratorNotNumeric NumeratorNegative state.favoriteNumerator

        parsedDenominator =
            tryParseInt DenominatorMissing DenominatorNotNumeric DenominatorNegative state.favoriteDenominator

        denominator =
            parsedDenominator
                |> Result.andThen
                    (\d ->
                        if d == 0 then
                            Err DenominatorZero

                        else
                            Ok d
                    )

        makeFraction n d =
            if d < n then
                Nothing

            else
                Just ( n, d )

        -- Put numerator and denominator together.
        -- Nothing means problem with prerequisites -- either
        -- numerator or denominator were missing.
        -- Just Nothing means a problem with the combination.
        -- Just (Just frac) means combination was successful.
        validFraction : Maybe (Maybe Fraction)
        validFraction =
            Maybe.map makeFraction (Result.toMaybe parsedNumerator)
                |> MaybeEx.andMap (Result.toMaybe denominator)
    in
    Form.Result.start FractionErrors identity
        |> checkErr parsedNumerator
        |> checkErr denominator
        |> maybeValid (validFraction |> MaybeEx.join)
        -- Don't show an improper fraction problem when there's
        -- problems with either field. In other words, show errors
        -- only if validFraction was `Just Nothing`.
        |> maybeErr (validFraction |> Maybe.andThen (nothingAs FractionImproper))
        |> Form.Result.toResult


{-| Update the form state according to form data.
I just wanted to separate this (relatively straightforward) part out
from the validation.
-}
applyMsg : Msg -> FormState -> FormState
applyMsg msg model =
    case msg of
        UpdateLiveValidate val ->
            { model | liveValidate = val }

        UpdateUsername val ->
            { model
                | username = val
                , errors = model.errors |> Maybe.map (\errors -> { errors | username = Nothing })
            }

        UpdatePassword val ->
            { model
                | password = val
                , errors = model.errors |> Maybe.map (\errors -> { errors | password = Nothing })
            }

        UpdateConfirmPassword val ->
            { model
                | confirmPassword = val
                , errors = model.errors |> Maybe.map (\errors -> { errors | confirmPassword = Nothing })
            }

        UpdateNumerator val ->
            { model
                | favoriteNumerator = val

                -- Yuck. In a real application, you might want to
                -- use a lens or something.
                , errors =
                    model.errors
                        |> Maybe.map
                            (\errors ->
                                { errors
                                    | fractionError =
                                        errors.fractionError
                                            |> Maybe.map
                                                (\frac ->
                                                    { frac
                                                        | numerator = Nothing

                                                        -- We have to remember also to clear the combination field when any of the fields in the fraction get updated.
                                                        , combination = Nothing
                                                    }
                                                )
                                }
                            )
            }

        UpdateDenominator val ->
            { model
                | favoriteDenominator = val
                , errors =
                    model.errors
                        |> Maybe.map
                            (\errors ->
                                { errors
                                    | fractionError =
                                        errors.fractionError
                                            |> Maybe.map
                                                (\frac ->
                                                    { frac
                                                        | denominator = Nothing

                                                        -- We have to remember also to clear the combination field when any of the fields in the fraction get updated.
                                                        , combination = Nothing
                                                    }
                                                )
                                }
                            )
            }

        UpdateAcceptTerms val ->
            { model
                | acceptTerms = val
                , errors = model.errors |> Maybe.map (\errors -> { errors | acceptTerms = Nothing })
            }

        -- Technically, this should probably happen *after* the
        -- validation, but whatever.
        SubmitClicked ->
            { model | submitAttempts = model.submitAttempts + 1 }

        ResetClicked ->
            initialFormState


flattenErrors : FormErrors -> Maybe FormErrors
flattenErrors errors =
    AnyJust.start FormErrors
        |> AnyJust.check errors.username
        -- We don't flatten errors.password because 1. we can't
        -- (all its fields are Bool, not Maybe) and 2. it's never
        -- "half present" -- when it gets cleared, the whole thing
        -- gets cleared.
        |> AnyJust.check errors.password
        |> AnyJust.check errors.confirmPassword
        |> AnyJust.check (Maybe.andThen flattenFractionErrors errors.fractionError)
        |> AnyJust.check errors.acceptTerms
        |> AnyJust.finish


flattenFractionErrors : FractionErrors -> Maybe FractionErrors
flattenFractionErrors errors =
    AnyJust.start FractionErrors
        |> AnyJust.check errors.numerator
        |> AnyJust.check errors.denominator
        |> AnyJust.check errors.combination
        |> AnyJust.finish


view model =
    let
        errorStyle =
            [ Attributes.style "color" "red" ]
    in
    div []
        [ h1 [] [ text "User registration form" ]
        , form []
            [ div []
                [ input
                    [ Attributes.type_ "checkbox"
                    , Attributes.checked model.formState.liveValidate
                    , Attributes.id "live-validate"
                    , Events.onCheck UpdateLiveValidate
                    ]
                    []
                , label [ Attributes.for "live-validate" ] [ text "Live validation" ]
                ]
            , div []
                [ div []
                    [ label [ Attributes.for "username" ]
                        [ text "Username"
                        , case Maybe.andThen .username model.formState.errors of
                            Just UsernameMissing ->
                                span errorStyle [ text "Please enter a username." ]

                            Just (UsernameInUse username) ->
                                span errorStyle [ text <| "The username " ++ username ++ " is already in use. Please choose another." ]

                            Nothing ->
                                text ""
                        ]
                    ]
                , div []
                    [ input [ Attributes.value model.formState.username, Attributes.id "username", Events.onInput UpdateUsername ] []
                    ]
                ]
            , div []
                [ div []
                    [ label [ Attributes.for "password" ]
                        [ text "Password"
                        , case Maybe.andThen .password model.formState.errors of
                            Just errors ->
                                ul errorStyle <|
                                    MaybeEx.values
                                        [ if errors.passwordMissing then
                                            Just (li [] [ text "Please enter a password." ])

                                          else
                                            Nothing
                                        , if errors.passwordNoLetter then
                                            Just (li [] [ text "Your password must contain at least one letter." ])

                                          else
                                            Nothing
                                        , if errors.passwordNoNumber then
                                            Just (li [] [ text "Your password must contain at least one number." ])

                                          else
                                            Nothing
                                        , if errors.passwordNoSymbol then
                                            Just (li [] [ text "Your password must contain at least one symbol." ])

                                          else
                                            Nothing
                                        ]

                            Nothing ->
                                text ""
                        ]
                    ]
                , div []
                    [ input [ Attributes.type_ "password", Attributes.value model.formState.password, Attributes.id "password", Events.onInput UpdatePassword ] []
                    ]
                ]
            , div []
                [ div []
                    [ label [ Attributes.for "confirm-password" ]
                        [ text "Confirm password"
                        , case Maybe.andThen .confirmPassword model.formState.errors of
                            Just ConfirmPasswordMissing ->
                                span errorStyle [ text "Please re-enter your password to ensure it is correct." ]

                            Just ConfirmPasswordDoesntMatch ->
                                span errorStyle [ text "Your password does not match the other password field." ]

                            Nothing ->
                                text ""
                        ]
                    ]
                , div []
                    [ input [ Attributes.type_ "password", Attributes.value model.formState.confirmPassword, Attributes.id "confirm-password", Events.onInput UpdateConfirmPassword ] []
                    ]
                ]
            , div []
                [ div []
                    [ label [ Attributes.for "favorite-numerator" ]
                        [ text "Favorite fraction (between 0 and 1)"
                        , case model.formState.errors |> Maybe.andThen .fractionError |> Maybe.andThen .numerator of
                            Just NumeratorMissing ->
                                span errorStyle [ text "Please enter a numerator." ]

                            Just NumeratorNotNumeric ->
                                span errorStyle [ text "Please enter a number for the numerator." ]

                            Just NumeratorNegative ->
                                span errorStyle [ text "Your numerator cannot be negative." ]

                            Nothing ->
                                text ""
                        , case model.formState.errors |> Maybe.andThen .fractionError |> Maybe.andThen .denominator of
                            Just DenominatorMissing ->
                                span errorStyle [ text "Please enter a denominator." ]

                            Just DenominatorNotNumeric ->
                                span errorStyle [ text "Please enter a number for the denominator." ]

                            Just DenominatorZero ->
                                span errorStyle [ text "Your denominator cannot be zero." ]

                            Just DenominatorNegative ->
                                span errorStyle [ text "Your denominator must be positive." ]

                            Nothing ->
                                text ""

                        -- Here's where we remember to consult the
                        -- "bad combination" fraction error. In this
                        -- case it wasn't that hard to remember
                        -- because these errors are not displayed next
                        -- to the input fields themselves.
                        , case model.formState.errors |> Maybe.andThen .fractionError |> Maybe.andThen .combination of
                            Just FractionImproper ->
                                span errorStyle [ text "The fraction you have entered is not between 0 and 1." ]

                            Nothing ->
                                text ""
                        ]
                    ]
                , div []
                    [ input [ Attributes.value model.formState.favoriteNumerator, Attributes.id "favorite-numerator", Events.onInput UpdateNumerator ] []
                    , text "/"
                    , input [ Attributes.value model.formState.favoriteDenominator, Attributes.id "favorite-denominator", Events.onInput UpdateDenominator ] []
                    ]
                ]
            , div []
                [ input
                    [ Attributes.type_ "checkbox"
                    , Attributes.checked model.formState.acceptTerms
                    , Attributes.id "accept-terms"
                    , Events.onCheck UpdateAcceptTerms
                    ]
                    []
                , label [ Attributes.for "accept-terms" ]
                    [ text "I accept the terms and conditions"
                    , case Maybe.andThen .acceptTerms model.formState.errors of
                        Just TermsNotAccepted ->
                            span errorStyle [ text "You must accept the terms and conditions." ]

                        Nothing ->
                            text ""
                    ]
                ]
            ]
        , div []
            [ button [ Events.onClick SubmitClicked, Attributes.disabled (MaybeEx.isJust model.formState.errors) ] [ text "Submit" ]
            , button [ Events.onClick ResetClicked ] [ text "Reset form" ]
            ]
        , div []
            [ text <| "Last successful submission: " ++ Debug.toString model.lastSuccess ]
        ]
