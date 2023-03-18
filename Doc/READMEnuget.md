# Jino

[![Jino on fuget.org](https://www.fuget.org/packages/Jino/badge.svg)](https://www.fuget.org/packages/Jino)
![code size](https://img.shields.io/github/languages/code-size/goswinr/Jino.svg) 
[![license](https://img.shields.io/github/license/goswinr/Jino)](LICENSE)

![Logo](https://raw.githubusercontent.com/goswinr/Jino/main/Doc/logo128.png)


Jino is a JSON parser and generator for F# on .NET and [Fable](https://fable.io/https://fable.io/).

The code and the tests are extracted and adapted from the much larger [FSharp.Data](https://github.com/fsprojects/FSharp.Data) library.
While there are many JSON libraries around, I haven't found one that runs on both .NET and [Fable](https://fable.io/). So I made this one by just copying the relevant part from [FSharp.Data](https://github.com/fsprojects/FSharp.Data) 

### Usage

##### Parsing

```fsharp
open Jino
let jValue = JsonValue.Parse(jsonAsString)
```

##### Generating

```fsharp
open Jino

let jValue = 
        JsonValue.Record [|
            "theAnswer", JsonValue.Number 42.0 
            "canBeFound", JsonValue.Bool true
            |]
        
jValue.ToFormattedString() // get a JSON string
```


### Documentation

For now see [full API documentation on fuget.org](https://www.fuget.org/packages/Jino)

### License
[MIT](https://raw.githubusercontent.com/goswinr/Jino/main/LICENSE.txt)

### Release Notes

`1.0.0`
- first public release