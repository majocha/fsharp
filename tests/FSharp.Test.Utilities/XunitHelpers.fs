namespace FSharp.Test

open System
open System.IO
open System.Text
open System.Threading

open Xunit.Sdk
open Xunit.Abstractions

module internal TestConsole =
    /// Redirects reads performed on different async execution contexts to the relevant TextReader held by AsyncLocal.
    type RedirectingTextReader(initial: TextReader) =
        inherit TextReader()
        let holder = AsyncLocal<_>()
        do holder.Value <- initial

        override _.Peek() = holder.Value.Peek()
        override _.Read() = holder.Value.Read()
        member _.Set (reader: TextReader) = holder.Value <- reader

    /// Redirects writes performed on different async execution contexts to the relevant TextWriter held by AsyncLocal.
    type RedirectingTextWriter(initial: TextWriter) =
        inherit TextWriter()
        let holder = AsyncLocal<_>()
        do holder.Value <- initial

        override _.Encoding = Encoding.UTF8
        override _.Write(value: char) = holder.Value.Write(value)
        override _.Write(value: string) = holder.Value.Write(value)
        override _.WriteLine(value: string) = holder.Value.WriteLine(value)
        member _.Value = holder.Value
        member _.Set (writer: TextWriter) = holder.Value <- writer

    let localIn = new RedirectingTextReader(TextReader.Null)
    let localOut = new RedirectingTextWriter(TextWriter.Null)
    let localError = new RedirectingTextWriter(TextWriter.Null)

    let initStreamsCapture () = 
        Console.SetIn localIn
        Console.SetOut localOut
        Console.SetError localError

    let resetWriters() =
        new StringWriter() |> localOut.Set
        new StringWriter() |> localError.Set

type TestConsole =
    static member OutText =
        Console.Out.Flush()
        string TestConsole.localOut.Value

    static member ErrorText =
        Console.Error.Flush()
        string TestConsole.localError.Value


/// Passes captured console output to xUnit.
type ConsoleCapturingTestRunner(test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource) =
    inherit XunitTestRunner(test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource)

    member _.BaseInvokeTestMethodAsync aggregator = base.InvokeTestMethodAsync aggregator
    override this.InvokeTestAsync (aggregator: ExceptionAggregator): Tasks.Task<decimal * string> =
        task {
            TestConsole.resetWriters()
            let! executionTime = this.BaseInvokeTestMethodAsync aggregator
            let output =
                seq {
                    TestConsole.OutText
                    if not (String.IsNullOrEmpty TestConsole.ErrorText) then
                        ""
                        "=========== Standard Error ==========="
                        ""
                        TestConsole.ErrorText
                } |> String.concat Environment.NewLine
            return executionTime, output
        }

/// Execute test cases one by one instead of all at once. Allow other collections to run simultaneously.
[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Method, AllowMultiple = false)>]
type RunInSequenceAttribute() = inherit Attribute()

type TestRun(sink) =
    inherit XunitTestFramework(sink)
    do
        // Init statics
        MessageSink.sinkWriter |> ignore
        TestConsole.initStreamsCapture()

    // Replace UniqueID to allow running all in parallel.
    let withNewUniqueId (testCase : IXunitTestCase) : ITestMethod =
        let oldTestMethod = testCase.TestMethod
        let oldTestClass = oldTestMethod.TestClass
        let oldTestCollection = oldTestMethod.TestClass.TestCollection

        // Create a new collection with a unique id for the test case.
        let newTestCollection =
                new TestCollection(
                    oldTestCollection.TestAssembly,
                    oldTestCollection.CollectionDefinition,
                    $"{oldTestCollection.DisplayName} {oldTestCollection.UniqueID}",
                    Guid.NewGuid()
                )

        // Duplicate the test and assign it to the new collection
        let newTestClass = new TestClass(newTestCollection, oldTestClass.Class)
        new TestMethod(newTestClass, oldTestMethod.Method)

    let canFullyParallelize (testCase : IXunitTestCase) =
        isNull testCase.TestMethod.TestClass.TestCollection.CollectionDefinition
        && testCase.TestMethod.TestClass.Class.GetCustomAttributes(typeof<Xunit.CollectionAttribute>) |> Seq.isEmpty
        && testCase.TestMethod.Method.GetCustomAttributes(typeof<RunInSequenceAttribute>) |> Seq.isEmpty
        && testCase.TestMethod.TestClass.Class.GetCustomAttributes(typeof<RunInSequenceAttribute>) |> Seq.isEmpty

    /// Make the test case run with ConsoleCapturingTestRunner and potentially allow parallel execution within single test module / class / theory.
    let rewrite (testCase : IXunitTestCase) : IXunitTestCase =
        let testMethod = if canFullyParallelize testCase then withNewUniqueId testCase else testCase.TestMethod

        match testCase with
        | :? XunitTheoryTestCase as testCase ->
            { new XunitTheoryTestCase(sink, testCase.DefaultMethodDisplay, testCase.DefaultMethodDisplayOptions, testMethod) with
                override _.RunAsync (sink, bus, args, aggregator, cts) =
                    let  runner : XunitTestCaseRunner =
                        { new XunitTheoryTestCaseRunner(testCase, testCase.DisplayName, testCase.SkipReason, args, sink, bus, aggregator, cts) with
                            override this.CreateTestRunner(test, bus, testCase, args, testMethod, methodArgs, skipReason, attrs, aggregator, cts) =
                                ConsoleCapturingTestRunner(test, bus, testCase, args, testMethod, methodArgs, skipReason, attrs, aggregator, cts)
                        }
                    runner.RunAsync()
            }
        | :? XunitTestCase as testCase ->
            { new XunitTestCase(sink, testCase.DefaultMethodDisplay, testCase.DefaultMethodDisplayOptions, testMethod) with
                override _.RunAsync (sink, bus, args, aggregator, cts) =
                    let  runner : XunitTestCaseRunner =
                        { new XunitTestCaseRunner(testCase, testCase.DisplayName, testCase.SkipReason, args, testCase.TestMethodArguments, bus, aggregator, cts) with
                            override this.CreateTestRunner(test, bus, testCase, args, testMethod, methodArgs, skipReason, attrs, aggregator, cts) =
                                ConsoleCapturingTestRunner(test, bus, testCase, args, testMethod, methodArgs, skipReason, attrs, aggregator, cts)
                        }
                    runner.RunAsync()
            }
        | _ -> testCase

    // Custom executor just to decorate test cases so that they run with our custom runner.
    override this.CreateExecutor assembly =
        {
            new XunitTestFrameworkExecutor(assembly, this.SourceInformationProvider, sink) with
                override this.RunTestCases (testCases, sink, executionOptions) =
                    base.RunTestCases(testCases |> Seq.map rewrite, sink, executionOptions)
        }
