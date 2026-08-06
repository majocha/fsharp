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

The initial F# surface supports the generic return shape
`System.Threading.Tasks.Task<'T>`. Other task-like shapes may be added later.

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
* The method must have a supported `Task<'T>` return shape.
* The method must be CIL and belong to an async-capable assembly.

## F# surface

The source-level marker is the compiler intrinsic `__runtimeAsync`. The
leading underscores make the low-level nature explicit and avoid collisions
with user code. It is not an attribute and is not an ordinary runtime
function.

The initial form is:

```fsharp
let f (x: int) : Task<int> =
    __runtimeAsync (
        let y = AsyncHelpers.Await (getValue x)
        y + 1)
```

The intrinsic marks the final logical result of a `Task<'T>`-returning method.
It is erased during code generation; the generated method returns `'T` at the
runtime-async `ret` instruction.

```fsharp
let addAsync (x: int) (y: int) : Task<int> =
    __runtimeAsync (
        let z = AsyncHelpers.Await (getAsync x)
        z + y)
```

There is no implicit awaiting of a task-returning expression:

```fsharp
let f () : Task<Task<int>> =
    __runtimeAsync (getValue ())
```

Flattening is achieved by an explicit await:

```fsharp
let f () : Task<int> =
    __runtimeAsync (AsyncHelpers.Await (getValue ()))
```

Builders can implement `Bind` by lowering their await operation to the same
runtime-async result pattern.

The component-test builder uses the corresponding delayed representation:
`RuntimeAsyncCode<'T>` is an alias for `unit -> 'T`. Its combinators are inline
and compose these functions; only inline `Run` introduces `__runtimeAsync` and
returns `Task<'T>`. This exercises the intended builder shape without making
the builder part of the initial public F# surface.

## Return-type inference

The intrinsic changes the method return convention, not the logical result
type of the expression.

Conceptually:

```text
expression:        'T
runtimeAsync:      Task<'T>
```

For a method:

```text
method result 'T becomes Task<'T>
```

This is not an ordinary F# cast. Type checking should infer the expression as
`T`, validate the `Task<'T>` return type, and record a runtime-async marker for
later code generation.

The declared `Task<'T>` result supplies the carrier while the intrinsic's
argument is checked as the logical `'T` result.

For members, use the normal tupled argument group used for .NET methods:

```fsharp
type C() =
    member this.Add(x: int, y: int) : Task<int> =
        __runtimeAsync (
            let z = AsyncHelpers.Await (getAsync x)
            z + y)
```

Tupled member arguments are preferred over curried member syntax because they
produce the expected .NET parameter list, overload behavior, reflection shape,
and interop surface.

The method return annotation supplies the `Task<'T>` carrier.

The intrinsic should validate the complete carrier type before generic
substitution. Only `Task<'T>` is valid in this initial implementation.

## Compiler representation

The typed tree and subsequent representation decisions need an internal
runtime-async marker distinct from legacy `Async<'T>` and resumable state
machines. The marker must survive the transformations that can produce a
method body:

* top-level functions;
* local functions and closures;
* delegate methods;
* instance and static members;
* virtual and interface implementations;
* generic and inline functions.

`__runtimeAsync` should be consumed during lowering. It must not become a
normal wrapper call, allocate an F# state machine, or use
`AsyncTaskMethodBuilder`/`AsyncValueTaskMethodBuilder`.

There are two lowering contexts:

* In a direct function or member declaration body, `__runtimeAsync` is a
  final-result marker. The compiler consumes it and marks the enclosing
  generated method.
* A value initializer that contains the marker needs a generated async helper,
  because a module initializer cannot itself be runtime-async.

Inlining needs an async-context boundary. A runtime-async body may be inlined
into another runtime-async method, but it must not be expanded into an
ordinary synchronous method if doing so introduces `AsyncHelpers` suspension
calls. Calls that are not safely inlined retain their runtime-async method
boundary.

## Type checking and diagnostics

The compiler should reject:

* use of `__runtimeAsync` outside a `Task<'T>`-returning method or generated
  helper;
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
* the method returns the selected `Task<'T>` shape;
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
* result-bearing `Task` calls;
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

1. Add the `__runtimeAsync` intrinsic and `Task<'T>`-aware type checking.
2. Add the internal typed-tree marker and preserve it through representation
   and inlining decisions.
3. Emit the `Async` method flag and runtime-compatible return-stack shape.
4. Emit and validate direct `AsyncHelpers` calls.
5. Add member, closure, delegate, generic, and inline handling.
6. Add compile-only, IL-verification, and capability-gated runtime tests.
7. Add builder support later by lowering `Bind` to the established
   runtime-async suspension representation.
