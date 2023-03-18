namespace Jino

// taken from 
// https://github.com/fsprojects/FSharp.Data/blob/main/src/FSharp.Data.Json.Core/JsonValue.fs
// and
// https://github.com/fsprojects/FSharp.Data/blob/main/src/FSharp.Data.Runtime.Utilities/TextConversions.fs

open System
open System.IO
open System.ComponentModel
open System.Globalization
open System.Runtime.InteropServices
open System.Text


type JsonParsingException(msg)=
    inherit Exception(msg)

type JsonValueAccessException(msg)=
    inherit Exception(msg)
    

/// Represents a JSON value. 
[<RequireQualifiedAccess>]
type JsonValue =
    | String  of string
    | Number  of float
    | Record  of properties: (string * JsonValue)[]
    | Array   of elements: JsonValue[]
    | Boolean of bool
    | Null  

    /// Returns the JSON string truncated to maximum 512 characters
    override this.ToString() =
        let str = this.ToFormattedString()
        if str.Length > 512 then
            str.Substring(0, 509) + "..."
        else
            str

    // --------------------------------------------------------------------------------------
    // JSON parser
    // --------------------------------------------------------------------------------------

    static member parse(jsonText: string) =

        let mutable i = 0
        let s = jsonText

        let buf = StringBuilder() // pre-allocate buffers for strings

        // Helper functions

        let isNumChar c =
            Char.IsDigit c
            || c = '.'
            || c = 'e'
            || c = 'E'
            || c = '+'
            || c = '-'

        let throw(doing:string) =
            let msg =
                sprintf
                    "Invalid JSON for %s. Starting at character %d, snippet = \n----\n%s\n-----\njson = \n------\n%s\n-------"
                    doing
                    i
                    (jsonText.[(max 0 (i - 10)) .. (min (jsonText.Length - 1) (i + 10))])
                    (if jsonText.Length > 1000 then
                        jsonText.Substring(0, 1000)
                    else
                        jsonText)
            raise (new JsonParsingException(msg))

        let ensure doing cond = if not cond then throw (doing)


        let rec skipCommentsAndWhitespace () =
            let skipComment () =
                // Supported comment syntax:
                // - // ...{newLine}
                // - /* ... */
                if i < s.Length && s.[i] = '/' then
                    i <- i + 1

                    if i < s.Length && s.[i] = '/' then
                        i <- i + 1

                        while i < s.Length && (s.[i] <> '\r' && s.[i] <> '\n') do
                            i <- i + 1
                    else if i < s.Length && s.[i] = '*' then
                        i <- i + 1

                        while i + 1 < s.Length
                            && s.[i] <> '*'
                            && s.[i + 1] <> '/' do
                            i <- i + 1

                        ensure "skipComment" (i + 1 < s.Length && s.[i] = '*' && s.[i + 1] = '/')
                        i <- i + 2

                    true

                else
                    false

            let skipWhitespace () =
                let initialI = i

                while i < s.Length && Char.IsWhiteSpace s.[i] do
                    i <- i + 1

                initialI <> i // return true if some whitespace was skipped

            if skipWhitespace () || skipComment () then
                skipCommentsAndWhitespace ()

        // Recursive descent parser for JSON that uses global mutable index
        let rec parseValue cont =
            skipCommentsAndWhitespace ()
            ensure "parseValue" (i < s.Length)

            match s.[i] with
            | '"' -> cont (JsonValue.String(parseString ()))
            | '-' -> cont (parseNum ())
            | c when Char.IsDigit(c) -> cont (parseNum ())
            | '{' -> parseObject cont
            | '[' -> parseArray cont
            | 't' -> cont (parseLiteral ("true", JsonValue.Boolean true))
            | 'f' -> cont (parseLiteral ("false", JsonValue.Boolean false))
            | 'n' -> cont (parseLiteral ("null", JsonValue.Null))
            | _ -> throw  "parseValue"

        and parseString () =
            ensure "parseString" (i < s.Length && s.[i] = '"')
            i <- i + 1

            while i < s.Length && s.[i] <> '"' do
                if s.[i] = '\\' then
                    ensure "parseString" (i + 1 < s.Length)

                    match s.[i + 1] with
                    | 'b' -> buf.Append('\b') |> ignore
                    | 'f' -> buf.Append('\f') |> ignore
                    | 'n' -> buf.Append('\n') |> ignore
                    | 't' -> buf.Append('\t') |> ignore
                    | 'r' -> buf.Append('\r') |> ignore
                    | '\\' -> buf.Append('\\') |> ignore
                    | '/' -> buf.Append('/') |> ignore
                    | '"' -> buf.Append('"') |> ignore
                    | 'u' ->
                        ensure "parseString" (i + 5 < s.Length)

                        let hexdigit d =
                            if d >= '0' && d <= '9' then int32 d - int32 '0'
                            elif d >= 'a' && d <= 'f' then int32 d - int32 'a' + 10
                            elif d >= 'A' && d <= 'F' then int32 d - int32 'A' + 10
                            else throw "hexdigit"

                        let unicodeChar (s: string) =
                            if s.Length <> 4 then throw "unicodeChar"

                            char (
                                hexdigit s.[0] * 4096
                                + hexdigit s.[1] * 256
                                + hexdigit s.[2] * 16
                                + hexdigit s.[3]
                            )

                        let ch = unicodeChar (s.Substring(i + 2, 4))
                        buf.Append(ch) |> ignore
                        i <- i + 4 // the \ and u will also be skipped past further below
                    | 'U' ->
                        ensure "unicodeChar" (i + 9 < s.Length)

                        let unicodeChar (s: string) =
                            if s.Length <> 8 then throw "unicodeChar"
                            if s.[0..1] <> "00" then throw "unicodeChar"
                            
                            let num = UInt32.Parse(s, NumberStyles.HexNumber)
                            // get Unicode Surrogate Pair n
                            // used http://en.wikipedia.org/wiki/UTF-16#Code_points_U.2B010000_to_U.2B10FFFF as a guide below
                            // only code points U+010000 to U+10FFFF supported
                            // for conversion to UTF16 surrogate pair
                            let codePoint = num - 0x010000u
                            let HIGH_TEN_BIT_MASK = 0xFFC00u // 1111|1111|1100|0000|0000
                            let LOW_TEN_BIT_MASK = 0x003FFu // 0000|0000|0011|1111|1111
                            let leadSurrogate = (codePoint &&& HIGH_TEN_BIT_MASK >>> 10) + 0xD800u
                            let trailSurrogate = (codePoint &&& LOW_TEN_BIT_MASK) + 0xDC00u
                            char leadSurrogate, char trailSurrogate


                        let lead, trail = unicodeChar (s.Substring(i + 2, 8))
                        buf.Append(lead) |> ignore
                        buf.Append(trail) |> ignore
                        i <- i + 8 // the \ and u will also be skipped past further below
                    | _ -> throw "parseString"

                    i <- i + 2 // skip past \ and next char
                else
                    buf.Append(s.[i]) |> ignore
                    i <- i + 1

            ensure "parseString" (i < s.Length && s.[i] = '"')
            i <- i + 1
            let str = buf.ToString()
            buf.Clear() |> ignore
            str

        and parseNum () =
            let start = i

            while i < s.Length && (isNumChar s.[i]) do
                i <- i + 1

            let len = i - start
            let sub = s.Substring(start, len)

            //match TextConversions.AsDecimal CultureInfo.InvariantCulture sub with
            //| Some x -> JsonValue.Number x
            //| _ ->
            match TextConversions.AsFloat false sub with
            | Some x -> JsonValue.Number x
            | _ ->  throw ("parseNumber '"+sub+"'") 

        and parsePair cont =
            let key = parseString ()
            skipCommentsAndWhitespace ()
            ensure "parsePair" (i < s.Length && s.[i] = ':')
            i <- i + 1
            skipCommentsAndWhitespace ()
            parseValue (fun v -> cont (key, v))

        and parseObject cont =
            ensure "parseObject" (i < s.Length && s.[i] = '{')
            i <- i + 1
            skipCommentsAndWhitespace ()
            let pairs = ResizeArray<_>()

            let parseObjectEnd () =
                ensure "parseObject" (i < s.Length && s.[i] = '}')
                i <- i + 1
                let res = pairs.ToArray() |> JsonValue.Record
                cont res

            if i < s.Length && s.[i] = '"' then
                parsePair (fun p ->
                    pairs.Add(p)
                    skipCommentsAndWhitespace ()

                    let rec parsePairItem () =
                        if i < s.Length && s.[i] = ',' then
                            i <- i + 1
                            skipCommentsAndWhitespace ()

                            parsePair (fun p ->
                                pairs.Add(p)
                                skipCommentsAndWhitespace ()
                                parsePairItem ())
                        else
                            parseObjectEnd ()

                    parsePairItem ())
            else
                parseObjectEnd ()

        and parseArray cont =
            ensure "parseArray" (i < s.Length && s.[i] = '[')
            i <- i + 1
            skipCommentsAndWhitespace ()
            let vals = ResizeArray<_>()

            let parseArrayEnd () =
                ensure  "parseArray" (i < s.Length && s.[i] = ']')
                i <- i + 1
                let res = vals.ToArray() |> JsonValue.Array
                cont res

            if i < s.Length && s.[i] <> ']' then
                parseValue (fun v ->
                    vals.Add(v)
                    skipCommentsAndWhitespace ()

                    let rec parseArrayItem () =
                        if i < s.Length && s.[i] = ',' then
                            i <- i + 1
                            skipCommentsAndWhitespace ()

                            parseValue (fun v ->
                                vals.Add(v)
                                skipCommentsAndWhitespace ()
                                parseArrayItem ())
                        else
                            parseArrayEnd ()

                    parseArrayItem ())
            else
                parseArrayEnd ()

        and parseLiteral (expected, r) =
            ensure "parseLiteral" (i + expected.Length <= s.Length)

            for j in 0 .. expected.Length - 1 do
                ensure  "parseLiteral" (s.[i + j] = expected.[j])

            i <- i + expected.Length
            r

        // Start by parsing the top-level value        
        let value = parseValue id
        skipCommentsAndWhitespace ()
        if i <> s.Length then throw ("parse")
        value

    /// Attempts to parse the specified JSON string
    static member tryParse(text:string) =
        try
            Some <| JsonValue.parse(text)
        with _ ->
            None  

    /// Serializes the JsonValue to the specified System.IO.TextWriter.
    member internal this.GetStringInternal(formatted) :string=
        let b = new StringBuilder()
        let write (x:'T) = b.Append x |> ignore<StringBuilder> 

        let newLine =
            if formatted then
                fun indentation plus ->
                    b.AppendLine() |> ignore<StringBuilder> 
                    System.String(' ', indentation + plus) |> write
            else
                fun _ _ -> ()

        let propSep = if formatted then "\": " else "\":"

        let rec serialize indentation =
            function
            | Null -> write "null"
            | Boolean b -> write(if b then "true" else "false")            
            | Number v when Double.IsInfinity v || Double.IsNaN v -> write "null"
            | Number number -> number.ToString(CultureInfo.InvariantCulture) |> b.Append |> ignore<StringBuilder> 
            | String s ->
                write "\""
                JsonValue.jsonStringEncodeTo b s
                write "\""
            | Record properties ->
                write "{"

                for i = 0 to properties.Length - 1 do
                    let k, v = properties.[i]
                    if i > 0 then write ","
                    newLine indentation 2
                    write "\""
                    JsonValue.jsonStringEncodeTo b k
                    write propSep
                    serialize (indentation + 2) v

                newLine indentation 0
                write "}"
            | Array elements ->
                write "["

                for i = 0 to elements.Length - 1 do
                    if i > 0 then write ","
                    newLine indentation 2
                    serialize (indentation + 2) elements.[i]

                if elements.Length > 0 then newLine indentation 0
                write "]"

        serialize 0 this
        b.ToString()

    // Encode characters that are not valid in JS string. The implementation is based
    // on https://github.com/mono/mono/blob/master/mcs/class/System.Web/System.Web/HttpUtility.cs
    static member internal jsonStringEncodeTo (b: StringBuilder) (value: string) =
        
        let write (x:'T) = b.Append x |> ignore<StringBuilder> 
        
        if not (String.IsNullOrEmpty value) then
            for i = 0 to value.Length - 1 do
                let c = value.[i]
                let ci = int c

                if ci >= 0 && ci <= 7
                   || ci = 11
                   || ci >= 14 && ci <= 31 then
                    write("\\u{0:x4}", ci) |> ignore
                else
                    match c with
                    | '\b' -> write "\\b"
                    | '\t' -> write "\\t"
                    | '\n' -> write "\\n"
                    | '\f' -> write "\\f"
                    | '\r' -> write "\\r"
                    | '"' -> write "\\\""
                    | '\\' -> write "\\\\"
                    | _ -> write c
    
    /// Gets the full JSON as nicely formatted string
    member this.ToFormattedString() :string = 
        this.GetStringInternal(true) 
    
    /// Gets the full JSON as string without any formatting or line breaks
    member this.ToCompactString()  :string =   
        this.GetStringInternal(false) 

    //-----------------------------------------------------
    // Static members
    //--------------------------------------------------------

    static member asString useNoneForNullOrEmpty  =
        function
        | JsonValue.String s ->
            if useNoneForNullOrEmpty && String.IsNullOrEmpty s then
                None
            else
                Some s
        | JsonValue.Boolean b -> Some(if b then "true" else "false")
        | JsonValue.Number n -> n.ToString(CultureInfo.InvariantCulture) |> Some
        | JsonValue.Null when not useNoneForNullOrEmpty -> Some ""
        | _ -> None

    static member asInteger  =
        function
        | JsonValue.Number f when            
            inRangeFloat Int32.MinValue Int32.MaxValue f
            && isIntegerFloat f
            ->
            Some(int f)
        | JsonValue.String s -> TextConversions.AsInteger  s
        | _ -> None

    static member asInteger64  =
        function
        | JsonValue.Number  f when
            inRangeFloat Int64.MinValue Int64.MaxValue f
            && isIntegerFloat f
            ->
            Some(int64 f)
        | JsonValue.String s -> TextConversions.AsInteger64  s
        | _ -> None

    static member asDecimal  =
        function
        | JsonValue.Number n -> Some (Decimal n)
        | JsonValue.String s -> TextConversions.AsDecimal  s
        | _ -> None

    static member asFloat  useNoneForMissingValues  =
        //static member asFloat missingValues useNoneForMissingValues  =
        function
        | JsonValue.Number n -> Some n
        | JsonValue.String s -> TextConversions.AsFloat useNoneForMissingValues  s
        | _ -> None

    static member asBoolean =
        function
        | JsonValue.Boolean b -> Some b
        | JsonValue.Number 1. -> Some true
        | JsonValue.Number 0. -> Some false
        | JsonValue.String s -> TextConversions.AsBoolean s
        | _ -> None

    static member asDateTimeOffset  =
        function
        | JsonValue.String s -> TextConversions.AsDateTimeOffset  s
        | _ -> None

    static member asDateTime  =
        function
        | JsonValue.String s -> TextConversions.AsDateTime  s
        | _ -> None

    static member asTimeSpan  =
        function
        | JsonValue.String s -> TextConversions.AsTimeSpan  s
        | _ -> None

    static member asGuid =
        function
        | JsonValue.String s -> TextConversions.AsGuid s
        | _ -> None

    //-----------------------------------------------------
    // Extension methods with operations on JSON values
    //--------------------------------------------------------

    /// Get a array of key-value pairs representing the properties of an object
    /// Orr an empty array if this is not a JsonValue.Record
    member this.Properties =
        match this with
        | JsonValue.Record properties -> properties
        | _ -> [||]

    /// Get property of a JSON object. Fails if the value is not an object
    /// or if the property is not present
    member this.GetProperty(propertyName) =
        match this with
        | JsonValue.Record properties ->
            match Array.tryFind (fst >> (=) propertyName) properties with
            | Some (_, value) -> value
            | None ->                
                properties
                |> Array.map fst
                |> String.concat "\r\n"
                |> sprintf "Property '%s' not found. Available properties are:\r\n%s" propertyName
                |> JsonValueAccessException
                |> raise
             
        | _ ->
            sprintf "This JSON Value is not an Object. Property '%s' can't be accessed on %s" propertyName (this.ToString())
            |> JsonValueAccessException
            |> raise
                       

    /// Try to get a property of a JSON value.
    /// Returns None if the value is not an object or if the property is not present.
    member this.TryGetProperty(propertyName) =
        match this with
        | JsonValue.Record properties ->
            Array.tryFind (fst >> (=) propertyName) properties
            |> Option.map snd
        | _ -> None

    /// Assuming the value is an object, get value with the specified name
    member this.Item(propertyName) =
        this.GetProperty(propertyName)

    /// Get all the elements of a JSON value.
    /// Returns an empty array if the value is not a JSON array.
    member this.AsArray =
        match this with
        | (JsonValue.Array elements) -> elements
        | _ -> [||]

    /// Get all the elements of a JSON value (assuming that the value is an array)
    member this.GetEnumerator() =
        this.AsArray.GetEnumerator()

    /// Try to get the value at the specified index, if the value is a JSON array.
    member this.Item(index) = 
        match this with
        | (JsonValue.Array xs) -> 
            if xs.Length > index then
                xs.[index]
            else
                sprintf "Index %d out of range for JSON.Array of %d items" index xs.Length
                |> JsonValueAccessException
                |> raise
        | _ -> 
            sprintf "This is not a JSON array. Index %d cannot be accessed on %s" index (this.ToString())
            |> JsonValueAccessException
            |> raise        
     

    /// Get the string value of an element (assuming that the value is a string , boolean or number)
    /// Returns the empty string for JsonValue.Null
    member this.AsString =
        match JsonValue.asString false this with
        | Some s -> s
        | _ ->
            sprintf "This is JSON value is not a string , boolean or number: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get a number as an integer (assuming that the value fits in integer)
    member this.AsInteger =
        match JsonValue.asInteger this with
        | Some i -> i
        | _ ->
            sprintf "Not an int: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get a number as a 64-bit integer (assuming that the value fits in 64-bit integer)
    member this.AsInteger64 =
        match JsonValue.asInteger64 this with
        | Some i -> i
        | _ ->
            sprintf "Not an int64: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get a number as a decimal (assuming that the value fits in decimal)
    member this.AsDecimal =
        match JsonValue.asDecimal this with
        | Some d -> d
        | _ ->
            sprintf "Not a decimal: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get a number as a float (assuming that the value is convertible to a float)
    member this.AsFloat =        
        //let missingValues = defaultArg missingValues TextConversions.DefaultMissingValues
        match JsonValue.asFloat  false this with
        | Some f -> f
        | _ ->
            sprintf "Not a float: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get the boolean value of an element (assuming that the value is a boolean)
    member this.AsBoolean =
        match JsonValue.asBoolean this with
        | Some b -> b
        | _ ->
            sprintf "Not a boolean: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get the DateTime value of an element (assuming that the value is a string
    /// containing well-formed ISO date or MSFT JSON date)
    member this.AsDateTime =
        match JsonValue.asDateTime this with
        | Some d -> d
        | _ ->
            sprintf "Not a DateTime: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get the DateTime offset value of an element (assuming that the value is a string
    /// containing well-formed ISO date time with offset or MSFT JSON DateTime with offset)
    member this.AsDateTimeOffset =
        match JsonValue.asDateTimeOffset this with
        | Some d -> d
        | _ ->
            sprintf "Not a DateTime offset: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get the TimeSpan value of an element (assuming that the value is a string
    /// containing well-formed time span)
    member this.AsTimeSpan =
        match JsonValue.asTimeSpan this with
        | Some t -> t
        | _ ->
            sprintf "Not a time span: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get the guid value of an element (assuming that the value is a guid)
    member this.AsGuid =
        match JsonValue.asGuid this with
        | Some g -> g
        | _ ->
            sprintf "Not a guid: %s" (this.ToString())
            |> JsonValueAccessException
            |> raise  

    /// Get inner text of an element
    member this.InnerText =
        match JsonValue.asString false this with
        | Some str -> str
        | None ->
            this.AsArray
            |> Array.map (fun e -> this.InnerText)
            |> String.Concat


    static member (?) (jsonObject: JsonValue, propertyName:string) = jsonObject.GetProperty(propertyName)
       
