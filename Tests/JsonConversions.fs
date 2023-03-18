module Jino.TestsConv

open NUnit.Framework
open FsUnit
open Jino

[<Test>]
let ``Boolean conversions``() = 
  let asBoolean = JsonValue.asBoolean

  JsonValue.Boolean true  |> asBoolean |> should equal (Some true)
  JsonValue.Number 1.     |> asBoolean |> should equal (Some true)
  JsonValue.String "Yes"  |> asBoolean |> should equal (Some true)
  JsonValue.String "True" |> asBoolean |> should equal (Some true)
  JsonValue.String "1"    |> asBoolean |> should equal (Some true)

  JsonValue.Boolean false  |> asBoolean |> should equal (Some false)
  JsonValue.Number 0.      |> asBoolean |> should equal (Some false)
  JsonValue.String "no"    |> asBoolean |> should equal (Some false)
  JsonValue.String "False" |> asBoolean |> should equal (Some false)
  JsonValue.String "0"     |> asBoolean |> should equal (Some false)

  JsonValue.Null          |> asBoolean |> should equal None
  JsonValue.Number 2.     |> asBoolean |> should equal None
  JsonValue.String "blah" |> asBoolean |> should equal None
