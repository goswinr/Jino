module Jino.TestsProps

open NUnit.Framework
open System
open Jino
open FsCheck

let escaped = Set(['t';'r';'b';'n';'f';'\\';'"'])

let rec isValidJsonString s = 
    match s with
    | [] -> true
    | ['\\'] -> false
    | '"' :: _t -> false
    | h :: _t when Globalization.CharUnicodeInfo.GetUnicodeCategory h 
                    = Globalization.UnicodeCategory.Control -> false
    | '\\' :: 'u' :: d1 :: d2 :: d3 :: d4 :: t 
        when [d1;d2;d3;d4] |> Seq.forall 
            (fun c -> (Char.IsDigit c || Text.RegularExpressions.Regex("[a-fA-F]").IsMatch(c.ToString())))
                -> isValidJsonString t
    | '\\' :: i :: _t when escaped |> (not << Set.contains i) -> false
    | '\\' :: i :: t when escaped |> Set.contains i -> isValidJsonString t
    | _h :: t -> isValidJsonString t 

let validJsonStringGen = 
    Arb.generate<string> 
    |> Gen.filter ((<>) null)
    |> Gen.filter (Seq.toList >> isValidJsonString)

let jsonValueGen : Gen<JsonValue> =  

    let stringValGen = Gen.map JsonValue.String validJsonStringGen
    let booleanValGen = Gen.map JsonValue.Boolean Arb.generate<bool>
    let nullValGen = Gen.constant JsonValue.Null
    let numberValGen = Gen.map JsonValue.Number Arb.generate<float>

    let recordGen record =
        record
        ||> Gen.map2 (fun x y -> (x,y)) 
        |> Gen.listOf
        |> Gen.map List.toArray
        |> Gen.map JsonValue.Record
    
    let rec tree() =
        let tree' s =
            match s with
            | 0 -> Gen.oneof [ booleanValGen; stringValGen; nullValGen; numberValGen ]
            | n when n>0 -> 
                let subtree = 
                    (validJsonStringGen, tree()) 
                    |> recordGen
                    |> Gen.resize (s|>float|>sqrt|>int) 
                let arrayGen =
                    tree()
                    |> Gen.listOf
                    |> Gen.map List.toArray
                    |> Gen.map JsonValue.Array
                    |> Gen.resize (s|>float|>sqrt|>int) 
                Gen.oneof [ subtree; arrayGen; booleanValGen; stringValGen; nullValGen; numberValGen ]
            | _ -> invalidArg "s" "Only positive arguments are allowed"
        Gen.sized tree'

    (validJsonStringGen, tree())
    |> recordGen

let jsonStringGen : Gen<string> =
 
    let validJsonStringGen' = 
        validJsonStringGen
        |> Gen.map (sprintf "\"%s\"")
    
    let boolGen = Gen.elements ["true"; "false"]
    let nullGen = Gen.constant "null"
    let numGen  = Arb.generate<decimal>
                  |> Gen.map (fun d -> d.ToString(System.Globalization.CultureInfo.InvariantCulture))

    let recordGen record =
        record
        ||> Gen.map2 (sprintf "{%s:%s}") 

    let rec tree() =
        let tree' s  =
            match s with
            | 0 -> Gen.oneof [validJsonStringGen'; boolGen; nullGen; numGen]
            | n when n>0 ->
                let subtree =
                    (validJsonStringGen', tree())
                    |> recordGen
                    |> Gen.resize (s|>float|>sqrt|>int)
                let arrayGen =
                    tree()
                    |> Gen.listOf
                    |> Gen.map (fun l -> sprintf "[%s]" (String.Join(",",l)))
                    |> Gen.resize (s|>float|>sqrt|>int) 
                Gen.oneof [ subtree;  arrayGen; validJsonStringGen'; boolGen; nullGen; numGen]
            | _ -> invalidArg "s" "Only positive arguments are allowed"
        Gen.sized tree'

    (validJsonStringGen', tree())
    |> recordGen

type Generators = 
    static member JsonValue() =
         {new Arbitrary<JsonValue>() with
            override x.Generator = jsonValueGen
            override x.Shrinker j = 
                match j with
                | JsonValue.Array elems -> elems :> seq<JsonValue>
                | JsonValue.Record [|prop|] -> Seq.singleton (prop |> snd)
                | JsonValue.Record props -> seq {for n in props -> JsonValue.Record([|n|])}
                | _ -> Seq.empty }

let unescape s =
    let convert (m:Text.RegularExpressions.Match) =
        match m.Value.[1] with
        | x when x = 'u' -> m.Value.Substring(2) |> (fun i -> Convert.ToInt32(i, 16)) |> char |> Char.ToString
        | x when x = 'U' -> m.Value.Substring(2) |> (fun i -> Convert.ToInt32(i, 16)) |> Char.ConvertFromUtf32
        | _ -> failwith "not a unicode literal"
    let r = new Text.RegularExpressions.Regex("(\\\\u[0-9a-fA-F]{4}|\\\\U[0-9a-fA-F]{8})")
    r.Replace(s, convert)

(* fails with Number infinity

[<Test>]
let  ``Parsing stringified JsonValue returns the same JsonValue`` () =
    Arb.register<Generators>() |> ignore

    let parseStringified (json: JsonValue) =
        json.ToCompactString()
        |> JsonValue.parse = json

    Check.One ({Config.QuickThrowOnFailure with MaxTest = 1000},
               parseStringified)


fails on too long float: "{"":0.4956514714004955621226912559}"

[<Test>]
let ``Stringifying parsed string returns the same string`` () =
    let stringifyParsed (s : string) =
        let jsonValue = JsonValue.parse s
        let jsonConverted = jsonValue.ToCompactString()
        if jsonConverted = s then
            true
        else
            unescape s = jsonConverted

    let jsonStringArb = Arb.fromGen (jsonStringGen)

    Check.One ({Config.QuickThrowOnFailure with MaxTest = 10000},
              (Prop.forAll jsonStringArb stringifyParsed))
*)

[<Test>]
let ``Stringifing parsed string returns the same string (unicode)`` () =
    let input = "{\"=\\u21DB1's8\":27670116}"
    //let input = "{\"=\\u21DB1's8\":27670116086083.0138369}"
    let jsonValue = JsonValue.parse input
    let jsonConverted = jsonValue.ToCompactString()
    Assert.AreNotEqual(input, jsonConverted) // this now has the escaped value
    Assert.AreEqual(unescape input, jsonConverted)

[<Test>]
let ``Stringifing parsed string returns the same string (UNICODE)`` () =
    let input = "{\"=\\U001000111's8\":27670116}"
    //let input = "{\"=\\U001000111's8\":27670116086083.0138369}"
    let jsonValue = JsonValue.parse input
    let jsonConverted = jsonValue.ToCompactString()
    Assert.AreNotEqual(input, jsonConverted) // this now has the escaped value
    Assert.AreEqual(unescape input, jsonConverted)

[<Test>]
let ``Single line is skipped`` () =
    let input =
        """//
        {
            "test": true // x
        }
        //"""
    let inputWithoutComment = """{"test": true}"""
    let jsonValue = JsonValue.parse input
    let jsonValueWithoutComment = JsonValue.parse inputWithoutComment
    Assert.AreEqual(jsonValueWithoutComment, jsonValue)

let ``Multiline comment is skipped`` () =
    let input =
        """/**/
        {"test": true}
        /**//**/ /**/ //
        /* {
                "test": true
            }
        */"""
    let inputWithoutComment = """{"test": true}"""
    let jsonValue = JsonValue.parse input
    let jsonValueWithoutComment = JsonValue.parse inputWithoutComment
    Assert.AreEqual(jsonValueWithoutComment, jsonValue)
