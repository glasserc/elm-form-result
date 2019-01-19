Yet another form helper library.

This library is a set of small utilities for handling forms in HTML.

The Form.Result philosophy:

- As much as possible, Form.Result leans into standard Elm types like
  `Result` and `Maybe`. This leads to flexibility (easy to use with
  whatever existing validation code you have) as well as making it a
  little easier to learn.
- Do just one thing. Form.Result does not try to render HTML or save
  you from having to write "boilerplate" update messages. If this is
  what you are after, you might like
  [composable-form](https://package.elm-lang.org/packages/hecrj/composable-form/latest/).
- Generic. Form.Result doesn't care what your error types are, how
  many you have, whether you re-use them, etc.

The main type is in Form.Result (which see), with some subsidiary
utilities in Form.Result.AnyJust and Form.Result.Utils.

A complete example of a form using this library is available in the
`examples` directory.

## Wait, what function do I want

This table provides a brief summary of the functions you can use to
add a field to a FormResult. I'd really rather put it in the module
documentation, but I couldn't figure out how to render a table in
Elmdoc. Leaving it here means it at least renders correctly on
[Github](https://github.com/glasserc/elm-form-result).

| Type to add    | Function to use  | Form fails validation if |
| -------------- | ---------------- | ------------------------ |
| Result err res | validated        | Result is Err            |
| Maybe err      | maybeErr         | Maybe is Just            |
| Maybe res      | maybeValid       | Maybe is Nothing         |
| Result err a   | checkErr         | Result is Err            |
| err, Maybe res | ifMissing        | Maybe is Nothing         |
| res            | unconditional    | never                    |
| err            | unconditionalErr | never                    |
