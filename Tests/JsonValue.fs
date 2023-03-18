module Jino.Tests

open NUnit.Framework
open System
open System.Globalization
open System.Threading
open NUnit.Framework
open FsUnit
open Jino


[<Test>]
let ``Can parse empty document``() =
    let j = JsonValue.parse "{}"
    j |> should equal (JsonValue.Record [| |])

[<Test>]
let ``Can parse document with single property``() =
    let j = JsonValue.parse "{\"firstName\": \"John\"}"
    j?firstName.AsString |> should equal "John"

[<Test>]
let ``Can parse document with text and integer``() =
    let j = JsonValue.parse "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25}"
    j?firstName.AsString |> should equal "John"
    j?lastName.AsString |> should equal "Smith"
    j?age.AsInteger  |> should equal 25

[<Test>]
let ``Can parse document with text and float``() =
    let j = JsonValue.parse "{\"firstName\": \"John\", \"lastName\": \"Smith\", \"age\": 25.25}"
    j?age.AsFloat |> should equal 25.25

[<Test>]
let ``Can parse document with date``() =
    let j = JsonValue.parse "{\"anniversary\": \"\\/Date(869080830450)\\/\"}"
    j?anniversary.AsDateTime |> should equal (new DateTime(1997, 07, 16, 19, 20, 30, 450, DateTimeKind.Utc))
    j?anniversary.AsDateTime.Kind |> should equal DateTimeKind.Utc

[<Test>]
let ``Can parse document with iso date``() =
    let j = JsonValue.parse "{\"anniversary\": \"2009-05-19 14:39:22.500\"}"
    j?anniversary.AsDateTime |> should equal (new DateTime(2009, 05, 19, 14, 39, 22, 500, DateTimeKind.Local))
    j?anniversary.AsDateTime.Kind |> should equal DateTimeKind.Local

let withCulture (cultureName: string) = 
    let originalCulture = CultureInfo.CurrentCulture;
    CultureInfo.CurrentCulture <- CultureInfo cultureName
    { new IDisposable with 
        member _.Dispose() = 
            CultureInfo.CurrentCulture <- originalCulture }

[<Test>]
let ``Can parse document with iso date in local culture``() =
    use _holder = withCulture "zh-CN"
    let j = JsonValue.parse "{\"anniversary\": \"2009-05-19 14:39:22.500\"}"
    j?anniversary.AsDateTime |> should equal (new DateTime(2009, 05, 19, 14, 39, 22, 500, DateTimeKind.Local))
    j?anniversary.AsDateTime.Kind |> should equal DateTimeKind.Local

[<Test>]
let ``Can parse document with partial iso date``() =
    let j = JsonValue.parse "{\"anniversary\": \"2009-05-19\"}"
    j?anniversary.AsDateTime |> should equal (new DateTime(2009, 05, 19, 0, 0, 0, DateTimeKind.Local))
    j?anniversary.AsDateTime.Kind |> should equal DateTimeKind.Local

[<Test>]
let ``Can parse document with timezone iso date``() =
    let j = JsonValue.parse "{\"anniversary\": \"2009-05-19 14:39:22+0600\"}"
    j?anniversary.AsDateTime.ToUniversalTime() |> should equal (new DateTime(2009, 05, 19, 8, 39, 22, DateTimeKind.Utc))

[<Test>]
let ``Can parse document with UTC iso date``() =
    let j = JsonValue.parse "{\"anniversary\": \"2009-05-19 14:39:22Z\"}"
    j?anniversary.AsDateTime.ToUniversalTime() |> should equal (new DateTime(2009, 05, 19, 14, 39, 22, DateTimeKind.Utc))
    j?anniversary.AsDateTime.Kind |> should equal DateTimeKind.Utc

[<Test>]
let ``Can parse document with timezone and fraction iso date``() =
    let j = JsonValue.parse "{\"anniversary\": \"1997-07-16T19:20:30.45+01:00\"}"
    j?anniversary.AsDateTime.ToUniversalTime() |> should equal (new DateTime(1997, 07, 16, 18, 20, 30, 450, DateTimeKind.Utc))

[<Test>]
let ``Can parse document with datetime offset as ticks and timezones`` () =
    let j = JsonValue.parse "{\"anniversary\": \"\\/Date(869080830450+1000)\\/\"}"
    j?anniversary.AsDateTimeOffset |> should equal <| DateTimeOffset (1997, 07, 16, 19, 20, 30, 450, TimeSpan.FromHours 10.)

[<Test>]
let ``Can parse document with datetime offset from iso date format``() =
    let j = JsonValue.parse "{\"anniversary\": \"2009-05-19 14:39:22+0600\"}"
    j?anniversary.AsDateTimeOffset |> should equal <| DateTimeOffset(2009, 05, 19, 14, 39, 22, TimeSpan.FromHours 6.)

[<Test; Explicit>]
let ``Can parse deep arrays``() =
    String.replicate 50000 "[" + String.replicate 50000 "]"
    |> JsonValue.parse
    |> ignore

[<Test; Explicit>]
let ``Can parse deep objects``() =
    (String.replicate 50000 "{\"a\":") + "\"\"" + (String.replicate 50000 "}")
    |> JsonValue.parse
    |> ignore

// TODO: Due to limitations in the current ISO 8601 datetime parsing these fail, and should be made to pass
//[<Test>]
//let ``Cant Yet parse document with basic iso date``() =
//    let j = JsonValue.parse "{\"anniversary\": \"19810405\"}"
//    j?anniversary.AsDateTime |> should equal (new DateTime(1981, 04, 05))
//
//[<Test>]
//let ``Cant Yet parse weird iso date``() =
//    let j = JsonValue.parse "{\"anniversary\": \"2010-02-18T16.5\"}"
//    j?anniversary.AsDateTime |> should equal (new DateTime(2010, 02, 18, 16, 30, 00))

[<Test>]
let ``Can parse completely invalid, but close, date as string``() =
    let j = JsonValue.parse "{\"anniversary\": \"2010-02-18T16.5:23.35:4\"}"
    (fun () -> j?anniversary.AsDateTime |> ignore) |> should throw typeof<JsonValueAccessException>
    j?anniversary.AsString |> should equal "2010-02-18T16.5:23.35:4"

//[<Test>]
//let ``Can parse positive time span with days and fraction``() =
//    let j = JsonValue.parse "{\"duration\": \"1:3:16:50.5\"}"
//    j?duration.AsTimeSpan |> should equal (TimeSpan(1, 3, 16, 50, 500))

[<Test>]
let ``Can parse positive time span without days and without fraction``() =
    let j = JsonValue.parse "{\"duration\": \"00:30:00\"}"
    j?duration.AsTimeSpan |> should equal (TimeSpan(0, 30, 0))

[<Test>]
let ``Can parse negative time span with days and fraction``() =
    let j = JsonValue.parse "{\"duration\": \"-1:3:16:50.5\"}"
    j?duration.AsTimeSpan |> should equal (TimeSpan(-1, -3, -16, -50, -500))

//[<Test>]
//let ``Can parse time span in different culture``() =
//    use _holder = withCulture "fr"
//    let j = JsonValue.parse("{\"duration\": \"1:3:16:50,5\"}")
//    j?duration.AsTimeSpan |> should equal (TimeSpan(1, 3, 16, 50, 500))

[<Test>]
let ``Can parse UTF-32 unicode characters`` () = 
  let j = JsonValue.parse """{ "value": "\U00010343\U00010330\U0001033F\U00010339\U0001033B" }"""
  j?value.AsString |> should equal "\U00010343\U00010330\U0001033F\U00010339\U0001033B"

(*  //FAILS

[<Test>]
let ``Can parse floats in different cultures``() =
    use _holder = withCulture "pt-PT"
    let j = JsonValue.parse "{ \"age\": 25.5}"
    j?age.AsFloat |> should equal 25.5
    let j = JsonValue.parse "{ \"age\": \"25.5\"}"
    j?age.AsFloat |> should equal 25.5
    let j = JsonValue.tryParse("{ \"age\": 25,5}")
    j |> should equal None
    let j = JsonValue.parse("{ \"age\": \"25,5\"}")
    j?age.AsFloat |> should equal 25.5


[<Test>]
let ``Can parse decimals in different cultures``() =
    use _holder = withCulture "pt-PT"
    let j = JsonValue.parse "{ \"age\": 25.5}"
    j?age.AsDecimal |> should equal 25.5m
    let j = JsonValue.parse "{ \"age\": \"25.5\"}"
    j?age.AsDecimal |> should equal 25.5m
    let j = JsonValue.tryParse("{ \"age\": 25,5}")
    j |> should equal None
    let j = JsonValue.parse("{ \"age\": \"25,5\"}")
    j?age.AsDecimal |> should equal 25.5m

[<Test>]
let ``Can parse dates in different cultures``() =
    use _holder = withCulture "pt-PT"
    let j = JsonValue.parse "{ \"birthdate\": \"01/02/2000\"}"
    j?birthdate.AsDateTime.Month |> should equal 1
    let j = JsonValue.parse("{ \"birthdate\": \"01/02/2000\"}")
    j?birthdate.AsDateTime.Month |> should equal 2
*)

[<Test>]
let ``Can parse nested document`` () =
    let j = JsonValue.parse "{ \"main\": { \"title\": \"example\", \"nested\": { \"nestedTitle\": \"sub\" } } }"
    let main = j?main
    main?title.AsString |> should equal "example"
    let nested = main?nested
    nested?nestedTitle.AsString |> should equal "sub"

[<Test>]
let ``Can parse document with booleans``() =
    let j = JsonValue.parse "{ \"hasTrue\": true, \"hasFalse\": false }"
    j?hasTrue.AsBoolean |> should equal true
    j?hasFalse.AsBoolean |> should equal false

[<Test>]
let ``Can parse document with guids``() =
    let j = JsonValue.parse "{ \"id\": \"{F842213A-82FB-4EEB-AB75-7CCD18676FD5}\" }"
    j?id.AsGuid |> should equal (Guid.Parse "F842213A-82FB-4EEB-AB75-7CCD18676FD5")

[<Test>]
let ``Can parse document with null``() =
    let j = JsonValue.parse "{ \"items\": [{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}] }"
    let jArray = j?items
    jArray.[0]?id.AsString |> should equal "Open"
    jArray.[1]               |> should equal JsonValue.Null
    jArray.[2]?id.AsString |> should equal "Pause"

[<Test>]
let ``Can parse array in outermost scope``() =
    let jArray = JsonValue.parse "[{\"id\": \"Open\"}, null, {\"id\": \"Pause\"}]"
    jArray.[0]?id.AsString |> should equal "Open"
    jArray.[1]               |> should equal JsonValue.Null
    jArray.[2]?id.AsString |> should equal "Pause"

[<Test>]
let ``Can parse a string from twitter api without throwing an error``() =
    let text =
        "[{\"in_reply_to_status_id_str\":\"115445959386861568\",\"truncated\":false,\"in_reply_to_user_id_str\":\"40453522\",\"geo\":null,\"retweet_count\":0,\"contributors\":null,\"coordinates\":null,\"user\":{\"default_profile\":false,\"statuses_count\":3638,\"favourites_count\":28,\"protected\":false,\"profile_text_color\":\"634047\",\"profile_image_url\":\"http:\\/\\/a3.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\",\"name\":\"Steffen Forkmann\",\"profile_sidebar_fill_color\":\"E3E2DE\",\"listed_count\":46,\"following\":true,\"profile_background_tile\":false,\"utc_offset\":3600,\"description\":\"C#, F# and Dynamics NAV developer, blogger and sometimes speaker. Creator of FAKE - F# Make and NaturalSpec.\",\"location\":\"Hamburg \\/ Germany\",\"contributors_enabled\":false,\"verified\":false,\"profile_link_color\":\"088253\",\"followers_count\":471,\"url\":\"http:\\/\\/www.navision-blog.de\\/blog-mitglieder\\/steffen-forkmann-ueber-mich\\/\",\"profile_sidebar_border_color\":\"D3D2CF\",\"screen_name\":\"sforkmann\",\"default_profile_image\":false,\"notifications\":false,\"show_all_inline_media\":false,\"geo_enabled\":true,\"profile_use_background_image\":true,\"friends_count\":373,\"id_str\":\"22477880\",\"is_translator\":false,\"lang\":\"en\",\"time_zone\":\"Berlin\",\"created_at\":\"Mon Mar 02 12:04:39 +0000 2009\",\"profile_background_color\":\"EDECE9\",\"id\":22477880,\"follow_request_sent\":false,\"profile_background_image_url_https\":\"https:\\/\\/si0.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_background_image_url\":\"http:\\/\\/a1.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_image_url_https\":\"https:\\/\\/si0.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\"},\"favorited\":false,\"in_reply_to_screen_name\":\"ovatsus\",\"source\":\"\\u003Ca href=\\\"http:\\/\\/www.tweetdeck.com\\\" rel=\\\"nofollow\\\"\\u003ETweetDeck\\u003C\\/a\\u003E\",\"id_str\":\"115447331628916736\",\"in_reply_to_status_id\":115445959386861568,\"id\":115447331628916736,\"created_at\":\"Sun Sep 18 15:29:23 +0000 2011\",\"place\":null,\"retweeted\":false,\"in_reply_to_user_id\":40453522,\"text\":\"@ovatsus I know it's not complete. But I don't want to add a dependency on FParsec in #FSharp.Data. Can you send me samples where it fails?\"},{\"in_reply_to_status_id_str\":null,\"truncated\":false,\"in_reply_to_user_id_str\":null,\"geo\":null,\"retweet_count\":0,\"contributors\":null,\"coordinates\":null,\"user\":{\"statuses_count\":3637,\"favourites_count\":28,\"protected\":false,\"profile_text_color\":\"634047\",\"profile_image_url\":\"http:\\/\\/a3.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\",\"name\":\"Steffen Forkmann\",\"profile_sidebar_fill_color\":\"E3E2DE\",\"listed_count\":46,\"following\":true,\"profile_background_tile\":false,\"utc_offset\":3600,\"description\":\"C#, F# and Dynamics NAV developer, blogger and sometimes speaker. Creator of FAKE - F# Make and NaturalSpec.\",\"location\":\"Hamburg \\/ Germany\",\"contributors_enabled\":false,\"verified\":false,\"profile_link_color\":\"088253\",\"followers_count\":471,\"url\":\"http:\\/\\/www.navision-blog.de\\/blog-mitglieder\\/steffen-forkmann-ueber-mich\\/\",\"profile_sidebar_border_color\":\"D3D2CF\",\"screen_name\":\"sforkmann\",\"default_profile_image\":false,\"notifications\":false,\"show_all_inline_media\":false,\"geo_enabled\":true,\"profile_use_background_image\":true,\"friends_count\":372,\"id_str\":\"22477880\",\"is_translator\":false,\"lang\":\"en\",\"time_zone\":\"Berlin\",\"created_at\":\"Mon Mar 02 12:04:39 +0000 2009\",\"profile_background_color\":\"EDECE9\",\"id\":22477880,\"default_profile\":false,\"follow_request_sent\":false,\"profile_background_image_url_https\":\"https:\\/\\/si0.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_background_image_url\":\"http:\\/\\/a1.twimg.com\\/images\\/themes\\/theme3\\/bg.gif\",\"profile_image_url_https\":\"https:\\/\\/si0.twimg.com\\/profile_images\\/1280550984\\/buddy_lueneburg_normal.jpg\"},\"favorited\":false,\"in_reply_to_screen_name\":null,\"source\":\"\\u003Ca href=\\\"http:\\/\\/www.tweetdeck.com\\\" rel=\\\"nofollow\\\"\\u003ETweetDeck\\u003C\\/a\\u003E\",\"id_str\":\"115444490331889664\",\"in_reply_to_status_id\":null,\"id\":115444490331889664,\"created_at\":\"Sun Sep 18 15:18:06 +0000 2011\",\"possibly_sensitive\":false,\"place\":null,\"retweeted\":false,\"in_reply_to_user_id\":null,\"text\":\"Added a simple Json parser to #FSharp.Data http:\\/\\/t.co\\/3JGI56SM - #fsharp\"}]"
    JsonValue.parse text |> ignore

[<Test>]
let ``Can parse array of numbers``() =
    let j = JsonValue.parse "[1, 2, 3]"
    j.[0] |> should equal (JsonValue.Number 1.)
    j.[1] |> should equal (JsonValue.Number 2.)
    j.[2] |> should equal (JsonValue.Number 3.)

[<Test>]
let ``Can parse array of numbers when culture is using comma as decimal separator``() =
    use _holder = withCulture "de-DE"
    CultureInfo.CurrentCulture.NumberFormat.CurrencyDecimalSeparator |> should equal ","
    let j = JsonValue.parse("[25,5,5,25]")
    j.[0] |> should equal (JsonValue.Number 25.)
    j.[1] |> should equal (JsonValue.Number 5.)
    j.[2] |> should equal (JsonValue.Number 5.)
    j.[3] |> should equal (JsonValue.Number 25.)

[<Test>]
let ``Quotes in strings are properly escaped``() =
    let jsonStr = "{\"short_description\":\"This a string with \\\"quotes\\\"\"}"
    let j = JsonValue.parse jsonStr
    j.ToCompactString()|> should equal jsonStr

[<Test>]
let ``Can parse simple array``() =
    let j = JsonValue.parse "[\"Adam\",\"Eve\",\"Bonnie\",\"Clyde\",\"Donald\",\"Daisy\",\"Han\",\"Leia\"]"
    j.[0] |> should equal (JsonValue.String "Adam")
    j.[1] |> should equal (JsonValue.String "Eve")
    j.[2] |> should equal (JsonValue.String "Bonnie")
    j.[3] |> should equal (JsonValue.String "Clyde")

[<Test>]
let ``Can parse nested array``() =
    let j = JsonValue.parse "[ [\"Adam\", \"Eve\"], [\"Bonnie\", \"Clyde\"], [\"Donald\", \"Daisy\"], [\"Han\", \"Leia\"] ]"
    j.[0].[0] |> should equal (JsonValue.String "Adam")
    j.[0].[1] |> should equal (JsonValue.String "Eve")
    j.[1].[0] |> should equal (JsonValue.String "Bonnie")
    j.[1].[1] |> should equal (JsonValue.String "Clyde")

[<Test>]
let ``TryParse is None when a bad JSON value is given``() =
    let j = JsonValue.tryParse "}{"
    j |> should equal None

[<Test>]
let ``TryParse is Some of the correct JSON value when a good structure is given``() =
    let j = JsonValue.tryParse """{ "foo": "bar" }"""
    j |> should equal (Some (JsonValue.Record [| "foo", JsonValue.String "bar" |]))

[<Test>]
let ``Can serialize empty document``() =
    (JsonValue.Record [| |]).ToCompactString()
    |> should equal "{}"

[<Test>]
let ``Can serialize document with single property``() =
    (JsonValue.Record [| "firstName", JsonValue.String "John" |]).ToCompactString()
    |> should equal "{\"firstName\":\"John\"}"

[<Test>]
let ``Can serialize document with booleans``() =
    ( JsonValue.Record [| "aa", JsonValue.Boolean true
                          "bb", JsonValue.Boolean false |] ).ToCompactString()
    |> should equal "{\"aa\":true,\"bb\":false}"

[<Test>]
let ``Can serialize document with array, null and number``() =
    let text = "{\"items\":[{\"id\":\"Open\"},null,{\"id\":25}]}"
    let json = JsonValue.parse text
    json.ToCompactString() |> should equal text

[<TestCase(Double.NaN)>]
[<TestCase(Double.PositiveInfinity)>]
[<TestCase(Double.NegativeInfinity)>]
let ``Serializes special float value as null`` v =
    let json = JsonValue.Number v
    json.ToCompactString() |> should equal "null"

let normalize (str:string) =
  str.Replace("\r\n", "\n")
     .Replace("\r", "\n")

[<Test>]
let ``Pretty printing works``() =
    let text = """{"items":[{"id":"Open"},null,{"id":25}],"x":{"y":2},"z":[]}"""
    let json = JsonValue.parse text
    json.ToString() |> normalize |> should equal (normalize """{
  "items": [
    {
      "id": "Open"
    },
    null,
    {
      "id": 25
    }
  ],
  "x": {
    "y": 2
  },
  "z": []
}""")

[<Test>]
let ``Can parse various JSON documents``() =
    let IsFloatNear (l : float) (r : float) =
        if r < 16. * Double.Epsilon then
            l < 16. * Double.Epsilon
        else
            let d = 1E-10
            let ratio = l / r
            ratio < (1. + d) && ratio > (1. - d)

    let rec IsJsonEqual (l : JsonValue) (r : JsonValue) =
        match l,r with
        | JsonValue.Null        , JsonValue.Null                                        -> true
        | JsonValue.Boolean lb  , JsonValue.Boolean rb when lb = rb                     -> true
        | JsonValue.String  ls  , JsonValue.String  rs when ls = rs                     -> true
        | JsonValue.Number   lf  , JsonValue.Number   rf when IsFloatNear lf rf           -> true // The reasons we do a custom isEqual
        | JsonValue.Number   lf  , JsonValue.Number  rd when IsFloatNear lf (float rd)   -> true // The reasons we do a custom isEqual
        | JsonValue.Number  ld  , JsonValue.Number   rf when IsFloatNear (float ld) rf   -> true // The reasons we do a custom isEqual
        | JsonValue.Number  ld  , JsonValue.Number  rd when ld = rd                     -> true // The reasons we do a custom isEqual
        | JsonValue.Array   lvs , JsonValue.Array   rvs ->
            if lvs.Length <> rvs.Length then false
            else
                let mutable res = true
                let mutable i   = 0
                let length      = lvs.Length
                while res && i < length do
                    let li = lvs.[i]
                    let ri = rvs.[i]
                    res <- IsJsonEqual li ri
                    i <- i + 1
                res
        | JsonValue.Record  lms , JsonValue.Record  rms ->
            if lms.Length <> rms.Length then false
            else
                let mutable res = true
                let mutable i   = 0
                let length      = lms.Length
                while res && i < length do
                    let (ln,li) = lms.[i]
                    let (rn,ri) = rms.[i]
                    res <- (ln = rn) && IsJsonEqual li ri
                    i <- i + 1
                res
        | _ -> false

    let Array   = JsonValue.Array
    let Null    = JsonValue.Null
    let Boolean = JsonValue.Boolean
    let String  = JsonValue.String
    let Record  = JsonValue.Record
    let Number = JsonValue.Number

    let manualTestCases =
        [
            // Positive tests
            """[]"""                                , Some <| Array [||]
            """[null]"""                            , Some <| Array [|Null|]
            """[true]"""                            , Some <| Array [|Boolean true|]
            """[false]"""                           , Some <| Array [|Boolean false|]
            """[""]"""                              , Some <| Array [|String ""|]
            """["Test"]"""                          , Some <| Array [|String "Test"|]
            """["Test\t"]"""                        , Some <| Array [|String "Test\t"|]
            """["\""]"""                            , Some <| Array [|String "\""|]
            """["\"\\\//\b\f\n\r\t\u2665"]"""       , Some <| Array [|String "\"\\//\b\f\n\r\t\u2665"|]
            """["\ud83d\udc36"]"""                  , Some <| Array [|String "\ud83d\udc36"|]
            """[0]"""                               , Some <| Array [|Number 0.|]
            """[0.5]"""                             , Some <| Array [|Number 0.5|]
            """[1234]"""                            , Some <| Array [|Number 1234.|]
            """[-1234]"""                           , Some <| Array [|Number -1234.|]
            """[1234.25]"""                         , Some <| Array [|Number 1234.25|]
            """[-1234.25]"""                        , Some <| Array [|Number -1234.25|]
            """[1234.50E2]"""                       , Some <| Array [|Number 123450.|]
            """[-1234.5E+2]"""                      , Some <| Array [|Number -123450.|]
            """[123450E-2]"""                       , Some <| Array [|Number 1234.50|]
            """[-123450e-2]"""                      , Some <| Array [|Number -1234.50|]
            """[4.26353146520608E+18]"""            , Some <| Array [|Number 4.26353146520608E+18|]
            """[1.2345]"""                          , Some <| Array [|Number 1.2345|]
            """[null,false]"""                      , Some <| Array [|Null;Boolean false|]
            """[{}]"""                              , Some <| Array [|Record [||]|]
            """{}"""                                , Some <| Record [||]
            """{"a":null}"""                        , Some <| Record [|"a",Null|]
            """{"a":[]}"""                          , Some <| Record [|"a",Array [||]|]
            """{"a":[],"b":{}}"""                   , Some <| Record [|"a",Array [||];"b",Record [||]|]
            """{"\"":null}"""                       , Some <| Record [|"\"",Null|]
            """{"\"\\\//\b\f\n\r\t\u2665":null}"""  , Some <| Record [|"\"\\//\b\f\n\r\t\u2665",Null|]
            // Whitespace cases
            """  []"""                              , Some <| Array [||]
            """[]  """                              , Some <| Array [||]
            """  []  """                            , Some <| Array [||]
            """[  true]"""                          , Some <| Array [|Boolean true|]
            """[true  ]"""                          , Some <| Array [|Boolean true|]
            """[  true  ]"""                        , Some <| Array [|Boolean true|]
            """[null,  true]"""                     , Some <| Array [|Null;Boolean true|]
            """[null  ,true]"""                     , Some <| Array [|Null;Boolean true|]
            """[null  ,  true]"""                   , Some <| Array [|Null;Boolean true|]
            """  {}"""                              , Some <| Record [||]
            """{}  """                              , Some <| Record [||]
            """  {}  """                            , Some <| Record [||]
            """{  "a":true}"""                      , Some <| Record [|"a",Boolean true|]
            """{"a":true  }"""                      , Some <| Record [|"a",Boolean true|]
            """{  "a":true  }"""                    , Some <| Record [|"a",Boolean true|]
            """{"a"  :true}"""                      , Some <| Record [|"a",Boolean true|]
            """{"a":  true}"""                      , Some <| Record [|"a",Boolean true|]
            """{"a"  :  true}"""                    , Some <| Record [|"a",Boolean true|]
            """{"a":[]  ,"b":{}}"""                 , Some <| Record [|"a",Array [||];"b",Record [||]|]
            """{"a":[],  "b":{}}"""                 , Some <| Record [|"a",Array [||];"b",Record [||]|]
            """{"a":[]  ,  "b":{}}"""               , Some <| Record [|"a",Array [||];"b",Record [||]|]
            """0"""                                 , Some <| Number 0.
            """1234"""                              , Some <| Number 1234.
            """true"""                              , Some <| Boolean true
            """false"""                             , Some <| Boolean false
            """null"""                              , Some <| Null
            """ "Test" """                          , Some <| String "Test"
            // Negative tests
            """[NaN]"""                             , None
            """[,]"""                               , None
            """[true,]"""                           , None
            """{,}"""                               , None
            """{{}}"""                              , None
            """{a:[]}"""                            , None
            """{"a":[],}"""                         , None
            """[+123]"""                            , None
            // According to json.org this should fail but currently don't
            // """[0123]"""                            , None
        ]

    let testCases = manualTestCases

    let failures = System.Text.StringBuilder ()

    let failure (m : string) =
        ignore <| failures.AppendLine m

    for json,expected in testCases do
        try
            let result = JsonValue.parse json
            match expected with
            | Some exp when IsJsonEqual exp result  -> ()
            | Some exp                              -> failure (sprintf "Parse succeeded but didn't produce expected value\nJSON:\n%s\nExpected:\n%A\nActual:\n%A" json exp result)
            | None                                  -> failure (sprintf "Parse succeeded but expected to fail\nJSON:\n%s\nActual:\n%A" json result)
        with
            | e ->
                match expected with
                | None      -> ()
                | Some exp  -> failure <| sprintf "Parse failed but expected to succeed\nJSON:\n%s\nExpected:\n%A\nException:\n%A" json exp e

    if failures.Length > 0 then
        Assert.Fail <| failures.ToString ()

// [<Test>]
// let ``Basic special characters encoded correctly`` () = 
//   let input = " \"quoted\" and \'quoted\' and \r\n and \uABCD "
//   let w = new Text.StringBuilder()
//   JsonValue.jsonStringEncodeTo w input
//   let expected = " \\\"quoted\\\" and 'quoted' and \\r\\n and \uABCD "
//   (w.ToString()) |> should equal expected
// 
// [<Test>]
// let ``Encoding of simple string is valid JSON`` () = 
//   let input = "sample \"json\" with \t\r\n \' quotes etc."
//   let w = new Text.StringBuilder()
//   JsonValue.jsonStringEncodeTo w input
//   let expected = "sample \\\"json\\\" with \\t\\r\\n ' quotes etc."
//   (w.ToString()) |> should equal expected
// 
// [<Test>]
// let ``Encoding of markup is not overzealous`` () =
//   let input = "<SecurityLabel><MOD>ModelAdministrators</MOD></SecurityLabel>"
//   let w = new Text.StringBuilder()
//   JsonValue.jsonStringEncodeTo w input
//   let expected = input // Should not escape </>
//   (w.ToString()) |> should equal expected
