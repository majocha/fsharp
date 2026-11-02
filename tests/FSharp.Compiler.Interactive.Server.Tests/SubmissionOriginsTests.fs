// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module FSharp.Compiler.Interactive.Server.Tests.SubmissionOriginsTests

open Xunit

open Microsoft.VisualStudio.FSharp.Interactive

let private originOf (path: string, line: int) = ValueSome(struct (path, line, false))

[<Fact>]
let ``submissions sent in a row each take their own origin`` () =
    let origins = SubmissionOrigins()
    origins.Add("a", "A.fsx", 10, false)
    origins.Add("b", "B.fsx", 20, false)

    Assert.Equal(originOf ("A.fsx", 10), origins.Take "a")
    Assert.Equal(originOf ("B.fsx", 20), origins.Take "b")

[<Fact>]
let ``text typed into the window takes no origin and leaves the sent ones alone`` () =
    let origins = SubmissionOrigins()
    origins.Add("let x =\r\n    1\r\n", "A.fsx", 3, false)

    Assert.Equal(ValueNone, origins.Take "typed by hand")
    Assert.Equal(originOf ("A.fsx", 3), origins.Take "let x =\n    1")

[<Fact>]
let ``a submission that never ran does not keep a later one from its origin`` () =
    let origins = SubmissionOrigins()
    origins.Add("never ran", "A.fsx", 1, false)
    origins.Add("ran", "B.fsx", 2, false)

    Assert.Equal(originOf ("B.fsx", 2), origins.Take "ran")
    Assert.Equal(ValueNone, origins.Take "never ran")

[<Fact>]
let ``the same text sent twice takes the origins in the order it was sent`` () =
    let origins = SubmissionOrigins()
    origins.Add("go", "A.fsx", 1, false)
    origins.Add("go", "B.fsx", 2, false)

    Assert.Equal(originOf ("A.fsx", 1), origins.Take "go")
    Assert.Equal(originOf ("B.fsx", 2), origins.Take "go")

[<Fact>]
let ``only the submission sent for debugging is debugged`` () =
    let origins = SubmissionOrigins()
    origins.Add("go", "A.fsx", 1, true)
    origins.Add("go", "A.fsx", 1, false)

    Assert.Equal(ValueSome(struct ("A.fsx", 1, true)), origins.Take "go")
    Assert.Equal(originOf ("A.fsx", 1), origins.Take "go")
