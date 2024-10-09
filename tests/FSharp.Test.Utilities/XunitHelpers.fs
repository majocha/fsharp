namespace FSharp.Test

open System
open System.IO
open System.Text
open System.Threading

open Xunit.Sdk
open Xunit.Abstractions

module internal ParallelConsole =
    /// Redirects reads performed on different threads or async execution contexts to the relevant TextReader held by AsyncLocal.
    type RedirectingTextReader(initial: TextReader) =
        inherit TextReader()
        let holder = AsyncLocal<_>()
        do holder.Value <- initial

        override _.Peek() = holder.Value.Peek()
        override _.Read() = holder.Value.Read()
        member _.Set (reader: TextReader) = holder.Value <- reader

    /// Redirects writes performed on different threads or async execution contexts to the relevant TextWriter held by AsyncLocal.
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

type Console =
    static member OutText =
        Console.Out.Flush()
        string ParallelConsole.localOut.Value

    static member ErrorText =
        Console.Error.Flush()
        string ParallelConsole.localError.Value


/// Passes captured console output to xUnit.
type CustomTestRunner(test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource) =
    inherit XunitTestRunner(test, messageBus, testClass, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource)

    member _.BaseInvokeTestMethodAsync aggregator = base.InvokeTestMethodAsync aggregator
    override this.InvokeTestAsync (aggregator: ExceptionAggregator): Tasks.Task<decimal * string> =
        task {
            ParallelConsole.resetWriters()
            let! executionTime = this.BaseInvokeTestMethodAsync aggregator
            let output =
                seq {
                    Console.OutText
                    if not (String.IsNullOrEmpty Console.ErrorText) then
                        ""
                        "=========== Standard Error ==========="
                        ""
                        Console.ErrorText
                } |> String.concat Environment.NewLine
            return executionTime, output
        }

type CustomTestCase(decorated: XunitTestCase) =
    interface IXunitTestCase with
        member this.DisplayName = decorated.DisplayName
            member this.InitializationException = decorated.InitializationException
        member this.Method = decorated.Method
        member this.SkipReason = decorated.SkipReason
        member this.TestMethod = decorated.TestMethod
        member this.TestMethodArguments = decorated.TestMethodArguments
        member this.Timeout =decorated.Timeout
        member this.Traits = decorated.Traits
        member this.SourceInformation
            with get() = decorated.SourceInformation
            and set(value) = decorated.SourceInformation <- value
        member this.UniqueID = decorated.UniqueID
        member this.Deserialize(serializationInfo: IXunitSerializationInfo) = decorated.Deserialize(serializationInfo)
        member this.Serialize(serializationInfo: IXunitSerializationInfo) = decorated.Serialize(serializationInfo)
        member this.RunAsync (sink, messageBus, constructorArguments, aggregator, cancellationTokenSource) =
            let  customTestCaseRunner : XunitTestCaseRunner =
                match decorated with
                | :? XunitTheoryTestCase ->
                    { new XunitTheoryTestCaseRunner(decorated, decorated.DisplayName, decorated.SkipReason, constructorArguments, sink, messageBus, aggregator, cancellationTokenSource) with
                        override this.CreateTestRunner(test, messageBus, testCase, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource) =
                            CustomTestRunner(test, messageBus, testCase, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource)
                    }
                | _ ->
                    { new XunitTestCaseRunner(decorated, decorated.DisplayName, decorated.SkipReason, constructorArguments, decorated.TestMethodArguments, messageBus, aggregator, cancellationTokenSource) with
                        override this.CreateTestRunner(test, messageBus, testCase, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource) =
                            CustomTestRunner(test, messageBus, testCase, constructorArguments, testMethod, testMethodArguments, skipReason, beforeAfterAttributes, aggregator, cancellationTokenSource)
                    }
            customTestCaseRunner.RunAsync()
            

type TestRun(sink) =
    inherit XunitTestFramework(sink)
    do
        ParallelConsole.initStreamsCapture()

    override this.CreateExecutor assembly =
        {
            new XunitTestFrameworkExecutor(assembly, this.SourceInformationProvider, this.DiagnosticMessageSink) with
                override _.Deserialize (value: string): Xunit.Abstractions.ITestCase =
                    match base.Deserialize value with
                    | :? XunitTestCase as xunitTestCase -> CustomTestCase xunitTestCase                     
                    | testCase -> testCase
        }

