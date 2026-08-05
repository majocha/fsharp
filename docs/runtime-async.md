---
title: Runtime async
category: Compiler Internals
categoryindex: 200
index: 375
---

# Runtime async

This document describes the initial design for F# support for the .NET
runtime-async feature. The .NET design is still evolving:

* [Runtime-async specification](https://github.com/dotnet/runtime/blob/main/docs/design/specs/runtime-async.md)
* [Runtime-async code-generation contract](https://github.com/dotnet/runtime/blob/main/docs/design/coreclr/botr/runtime-async-codegen.md)

The first implementation targets functions, lambdas, members, and generated
methods. Computation-expression builders are deliberately deferred.

## Runtime contract

Runtime-async methods are CIL methods marked with
`MethodImplOptions.Async` (`0x2000`). The runtime, rather than a compiler
generated state machine and method builder, owns suspension and resumption.

The supported return shapes are:

* `System.Threading.Tasks.Task`
* `System.Threading.Tasks.ValueTask`
* `System.Threading.Tasks.Task<'T>`
* `System.Threading.Tasks.ValueTask<'T>`

For a non-generic return, the evaluation stack must be empty at `ret`. For a
generic return, the result type must be on the stack at `ret`.

Suspension is explicit. It is performed by calls to the appropriate
`System.Runtime.CompilerServices.AsyncHelpers` method:

* `Await` for `Task`, `ValueTask`, and configured awaitables
* `AwaitAwaiter` or `UnsafeAwaitAwaiter` for awaiters

The runtime specification recommends preserving the adjacent IL sequence:

```il
call Task<int32> SomeAsyncMethod(...)
call int32 AsyncHelpers::Await<int32>(Task<int32>)
```

Runtime-async methods currently have important restrictions:

* `tail.` and `localloc` are forbidden.
* Suspension cannot occur inside exception-handling regions.
* Byref, byref-like, and pinned locals cannot be preserved across suspension.
* The method must have a supported `Task` or `ValueTask` return shape.
* The method must be CIL and belong to an async-capable assembly.

## F# surface

The source-level marker is the compiler intrinsic `__runtimeAsync`. The
leading underscores make the low-level nature explicit and avoid collisions
with user code. It is not an attribute and is not an ordinary runtime
function.

The initial form is:

```fsharp
let f =
    __runtimeAsync<Task<_>> (fun x ->
        let y = AsyncHelpers.Await (getValue x)
        y + 1)
```

`ValueTask` is selected by specifying the complete return shape:

```fsharp
let f =
    __runtimeAsync<ValueTask<_>> (fun x ->
        let y = AsyncHelpers.Await (getValue x)
        y + 1)
```

Multi-argument lambdas are supported. The intrinsic consumes the complete
lambda argument spine and applies the carrier to the final result; it should
not force the arguments into a tuple:

```fsharp
let addAsync =
    __runtimeAsync<Task<_>> (fun (x: int) (y: int) ->
        let z = AsyncHelpers.Await (getAsync x)
        z + y)

// int -> int -> Task<int>
```

The compiler should preserve this shape where possible. A partially applied
runtime-async function may need an ordinary synchronous closure, but the
fully applied method that returns `Task` or `ValueTask` is the method marked
runtime-async.

For non-generic results, the corresponding forms are
`__runtimeAsync<Task>` and `__runtimeAsync<ValueTask>`. The logical result of
the lambda must be `unit`.

There is no `let!` syntax in this initial design. The lambda body is an
ordinary F# expression, and suspension points are explicit
`AsyncHelpers` calls. There is no implicit awaiting of a task-returning
expression:

```fsharp
__runtimeAsync<Task<_>> (fun () -> getValue ()) // Task<Task<int>>
```

Flattening is achieved by an explicit await:

```fsharp
__runtimeAsync<Task<_>> (fun () ->
    AsyncHelpers.Await (getValue ()))
```

Builders can later implement `Bind` by lowering their await operation to the
same runtime-async call pattern.

## Return-type inference

The intrinsic changes the *function return carrier*, not the logical result
type of the lambda.

Conceptually:

```text
lambda body:       'T
runtimeAsync<Task>: Task<'T>
runtimeAsync<ValueTask>: ValueTask<'T>
```

For a function argument:

```text
('A -> 'T) becomes ('A -> Task<'T>)
```

This is not an ordinary F# cast. Type checking should infer the lambda body
as `T`, validate the selected carrier, and record a runtime-async marker for
later code generation.

An expected type can select the carrier:

```fsharp
let f : int -> Task<int> =
    __runtimeAsync (fun x ->
        let y = AsyncHelpers.Await (getValue x)
        y + 1)
```

For members, use the normal tupled argument group used for .NET methods:

```fsharp
type C() =
    member this.Add(x: int, y: int) : Task<int> =
        __runtimeAsync (fun () ->
            let z = AsyncHelpers.Await (getAsync x)
            z + y)
```

The member return annotation selects the carrier. The corresponding
`ValueTask` form is:

```fsharp
member this.AddValue(x: int, y: int) : ValueTask<int> =
    __runtimeAsync (fun () ->
        let z = AsyncHelpers.Await (getAsync x)
        z + y)
```

Tupled member arguments are preferred over curried member syntax because they
produce the expected .NET parameter list, overload behavior, reflection shape,
and interop surface.

When no expected type is available, an explicit type argument should be
required. This avoids silently choosing `Task` when the caller intended
`ValueTask`.

The intrinsic should validate the complete carrier type before generic
substitution. Only the four runtime-supported shapes are valid.

## Compiler representation

The typed tree and subsequent representation decisions need an internal
runtime-async marker distinct from legacy `Async<'T>` and resumable state
machines. The marker must survive the transformations that can produce a
method body:

* top-level functions;
* local functions and closures;
* `FSharpFunc` implementations;
* delegate methods;
* instance and static members;
* virtual and interface implementations;
* generic and inline functions.

`__runtimeAsync` should be consumed during lowering. It must not become a
normal wrapper call, allocate an F# state machine, or use
`AsyncTaskMethodBuilder`/`AsyncValueTaskMethodBuilder`.

There are two lowering contexts:

* In a function-value context, `__runtimeAsync` produces a runtime-async
  function value. Closure conversion and partial application may introduce
  synchronous wrappers, but the final `Task`/`ValueTask`-returning method is
  marked runtime-async.
* In a direct function or member declaration body, `__runtimeAsync` is a body
  marker. The compiler consumes it and marks the enclosing generated method,
  avoiding a zero-argument closure that captures `this` and the method
  arguments.

The second form is intentionally declaration-context-sensitive. It gives
members normal .NET signatures while retaining the intrinsic function form
needed for lambdas and escaping function values.

Inlining needs an async-context boundary. A runtime-async body may be inlined
into another runtime-async method, but it must not be expanded into an
ordinary synchronous method if doing so introduces `AsyncHelpers` suspension
calls. Calls that are not safely inlined retain their runtime-async method
boundary.

## Type checking and diagnostics

The compiler should reject:

* use of `__runtimeAsync` with a non-lambda form until existing function
  values can be proven to have a suitable runtime-async body;
* unsupported return carriers;
* `AsyncHelpers` suspension calls outside a runtime-async method;
* suspension in exception-handling regions;
* unsupported byref, byref-like, pinned-local, `tail.`, and `localloc`
  situations;
* compilation when the referenced runtime libraries do not expose
  `MethodImplOptions.Async` and `AsyncHelpers`.

The compiler already probes target-runtime metadata through `InfoReader`.
Runtime-async support uses the `MethodImplOptions.Async` field exposed by
.NET 11, together with the `AsyncHelpers` API used by suspension points, rather
than hard-coding an SDK or runtime version.

## Runtime and SDK compatibility

On .NET 11, projects must opt in to Runtime Async with
`<Features>runtime-async=on</Features>`. This is separate from F# preview
language support; `EnablePreviewFeatures` is not required by .NET 11 for the
runtime feature itself.

The SDK selected by `global.json` is not sufficient to establish runtime-async
support. Three versions can differ:

1. The SDK used to build the F# compiler and tests.
2. The reference assemblies used while compiling generated F# code.
3. The CoreCLR or NativeAOT host and JIT that execute that code.

All three must be compatible for behavioral tests. In particular, compiling
against new reference assemblies and executing on an older host must not be
treated as a supported configuration.

The runtime implementation is changing over time. The current runtime sources
contain `MethodImplOptions.Async` and `AsyncHelpers`, but the `AsyncHelpers`
implementation is target-specific and unsupported targets may contain throwing
stubs. The runtime sources also contain internal test/build hooks such as
`RuntimeAsyncMethodGenerationAttribute`; those hooks are not the F# contract
unless the runtime team explicitly makes them one.

Build and test infrastructure must therefore:

* pin or identify the runtime commit used for runtime-async execution tests;
* use the matching reference assemblies when compiling those tests;
* run a capability probe before behavioral tests;
* avoid assuming that the repository's selected SDK implies JIT support;
* keep compile-only and runtime-execution tests separate;
* skip or produce a deliberate diagnostic on unsupported hosts;
* track changes in runtime build switches and enablement requirements rather
  than embedding version checks in the F# compiler.

The capability probe should verify the actual execution environment, including
`MethodImplOptions.Async`, `AsyncHelpers`, and a minimal marked method that
suspends and resumes. A metadata-only probe is not enough to prove JIT support.

## Test plan

### Compiler and IL tests

Compile source containing `__runtimeAsync` and verify:

* the generated method has the `Async` method implementation flag;
* the method returns the selected `Task` or `ValueTask` shape;
* no F# state-machine type or method builder is generated;
* direct async calls are followed by the appropriate `AsyncHelpers.Await`;
* closure, member, delegate, generic, and inline representations retain the
  marker;
* invalid carriers and restricted constructs produce diagnostics.

These tests can run as compile-only or IL-verification tests, but they must not
be interpreted as proof that the output executes on the current host runtime.

### Runtime tests

Run generated assemblies only on a compatible CoreCLR or NativeAOT runtime.
Cover at least:

* an already-completed `Task`;
* a genuinely suspended and resumed `Task`;
* `ValueTask` and result-bearing variants;
* multiple suspension points;
* captured locals and closure values;
* exceptions and cancellation;
* execution and synchronization context behavior;
* nested runtime-async calls;
* direct calls from synchronous methods.

Runtime tests should follow the runtime repository's capability-gated pattern
and record enough SDK/runtime information to reproduce failures. Keep these
tests in compiler component tests so they compile with the compiler under test,
not with the SDK compiler used to build the test assembly.

## Implementation order

1. Add the `__runtimeAsync` intrinsic and carrier-aware type checking.
2. Add the internal typed-tree marker and preserve it through representation
   and inlining decisions.
3. Emit the `Async` method flag and runtime-compatible return-stack shape.
4. Emit and validate direct `AsyncHelpers` calls.
5. Add member, closure, delegate, generic, and inline handling.
6. Add compile-only, IL-verification, and capability-gated runtime tests.
7. Add builder support later by lowering `Bind` to the established
   runtime-async suspension representation.
