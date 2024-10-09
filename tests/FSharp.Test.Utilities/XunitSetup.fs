namespace FSharp.Test

open Xunit

[<CollectionDefinition(nameof DoNotRunInParallel, DisableParallelization = true)>]
type DoNotRunInParallel = class end

module XUnitSetup =

    [<assembly: TestFramework("FSharp.Test.TestRun", "FSharp.Test.Utilities")>]
    do ()
