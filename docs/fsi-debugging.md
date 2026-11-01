---
title: F# Interactive Debugging
category: Compiler Internals
categoryindex: 200
index: 376
---
# Debugging code in F# Interactive

How a debugger gets from a running F# Interactive session to the user's source, what each piece is responsible for, and what is still missing. Tracking issue: [#17306](https://github.com/dotnet/fsharp/issues/17306). Read [F# Interactive Emit](fsi-emit.md) and [Debug emit](debug-emit.md) first.

## The pipeline

```
editor / interactive window  ──►  fsi process  ──►  submission assembly + PDB  ──►  debugger
 (host: picks SDK, attaches)      (compiles each      (loaded from memory)          (VS, attached
                                   submission)                                       to the fsi PID)
```

1. **The host** starts fsi, sends code, and attaches the debugger to the process that evaluates code.
   - Legacy VS window: `vsintegration/src/FSharp.VS.FSI` talks to fsi over stdin/stdout. "Debug in Interactive" (`fsiSessionToolWindow.fs`) attaches through DTE `Process.Attach()` and sends the selection behind a `#dbgbreak` directive and a `# <line> "<file>"` line directive.
   - JSON-RPC server (`src/fsi/fsiserver.fs`, protocol in `src/fsi/interactiveProtocol.fs`, .NET only): `fsi/initialize` reports `processId`, the process a debugger attaches to. `fsi/execute` takes `code`, an optional `sourcePath`/`startLine` (turned into a line directive) and `debug`.
   - New VS window over JSON-RPC: [PR #20565](https://github.com/dotnet/fsharp/pull/20565), not merged.
2. **fsi** (`src/Compiler/Interactive/fsi.fs`) compiles each submission. With `--multiemit+`, the default on .NET, every submission becomes its own assembly, written by `ilwrite.fs` and loaded with `Assembly.Load(assemblyBytes, pdbBytes)` (`FsiDynamicCompiler.EmitInMemoryAssembly`).
3. **The PDB** (`src/Compiler/AbstractIL/ilwritepdb.fs`) maps IL to documents and lines. Its document names come from the ranges of the parsed code: the script's path when a line directive names it, otherwise the name passed to `EvalInteraction` (`input.fsx`, `stdin.fsx`) or `stdin` for the console. They are resolved against `tcGlobals.directoryToResolveRelativePaths` (`TcGlobals.mkSourceDoc`).
4. **The debugger** reads symbols from memory, finds each document on disk or embedded in the PDB, and decides whether code is "user code" (Just My Code needs non-optimized modules with symbols).

## What makes a submission debuggable

| Need | Where | Notes |
|---|---|---|
| Non-optimized IL, no tail calls | `tcConfigB.optSettings`, `emitTailcalls`; read per submission through `TcConfig.Create(tcConfigB)` in `ProcessInputs` | fsi presets `--optimize+ -g --tailcalls+` (search `Preset:` in fsi.fs). |
| JIT optimizer off | `DebuggableAttribute(DisableOptimizations)` on the submission manifest when local optimizations are off ([#19921](https://github.com/dotnet/fsharp/pull/19921)) | Multi-emit only. With `--multiemit-`, the single dynamic assembly gets its attribute once, at session start. |
| Symbols | Portable PDB per submission, passed to `Assembly.Load` and written to the session's scripting-symbols temp directory | Both are needed. The runtime uses the in-memory copy for stack traces. Visual Studio finds no symbols without the file: no breakpoint binds and stepping leaves the submission. |
| Source | Document on disk, or source embedded in the PDB | See below. |
| A first stop | `#dbgbreak` / `debug = true` inject `if Debugger.IsAttached then Debugger.Break()` before the first breakable line (`FsiDynamicCompiler.CreateDebuggerBreak`) | The `IsAttached` guard matters: an unguarded `Debugger.Break()` terminates a .NET process with no debugger. |

### Source for code with no file

Code typed into an interactive window has no file behind it. When `FsiInteractionProcessor.EvalInteraction` gets a file name that does not exist on disk and debug info is on, it wraps the evaluation in `FsiDynamicCompiler.EmbeddingSource`. While that runs, the PDB writer receives an `InMemorySourceDocument` for that IL document. The writer:

- records the document under `<scripting symbols dir>\<n>\<file name>`, unique per `EvalInteraction` call, because a debugger identifies documents by path and every submission would otherwise claim the same `input.fsx`;
- checksums the text (UTF-8);
- embeds the text.

Diagnostics keep the original file name; only the PDB, and therefore stack traces, show the unique name.

Not covered:

- **Console stdin.** Its lines keep counting across submissions, or reset with `# 1 "stdin"` from the legacy VS window, so one submission's text does not line up with the line numbers in its PDB.
- **A `sourcePath` that does not exist** (an unsaved script). The embedded text includes the line directive the server prepends, so lines are off by one.

### Per-submission debugging

`fsi/execute` with `debug = true` calls the internal `FsiEvaluationSession.EvalInteractionNonThrowing(code, scriptFileName, debug, cancellationToken)`. For that call only, `withDebuggableCode` switches `tcConfigB` to `--optimize- --tailcalls- --debug:pdbonly`, restoring the saved fields afterwards, and the interactive state's `debugBreak` is set. The flag is cleared afterwards even when the code had no breakable line, so the break never leaks into a later submission. This replaces the legacy window's session-wide "debug mode" option and the session restart it needs.

## Status

Branch `fsi-debugging-prototype` on [majocha/fsharp](https://github.com/majocha/fsharp/tree/fsi-debugging-prototype), based on `main` at `fe180deb55`, implements:

- embedded sources for in-memory documents;
- the `debug` field on `fsi/execute`;
- the guarded break.

Tests:

- `tests/FSharp.Compiler.ComponentTests/Scripting/Interactive.fs`, module `PortablePdb`:
  - PDB round trip of an in-memory document;
  - unique document names, and line numbers from in-memory symbols, through a stack trace;
  - the PDB file is written unless debug info is off.
- `tests/FSharp.Compiler.Interactive.Server.Tests/FsiJsonRpcServerTests.fs`, section "Debugging": a debug submission is compiled unoptimized, the next one is not.

Verified by driving Visual Studio 2026 Insiders (18.12) through DTE, in a clean hive, against a JSON-RPC session with `debug = true`:

- the guarded break stops in the submission;
- code with no file opens from the source embedded in the PDB, on the right lines;
- code with a `sourcePath` opens the real file, and a breakpoint set there binds;
- stepping walks the submission's lines, over and into its functions.

How symbols reach VS decides whether any of this works:

| Symbols | Just My Code on | Just My Code off |
|---|---|---|
| PDB file, plus bytes to `Assembly.Load` (what fsi does) | works | works |
| Embedded in the assembly, no file | works | not loaded |
| Bytes to `Assembly.Load` only, no file | not loaded | not loaded |

When symbols are not loaded, breakpoints stay unbound and stepping leaves the submission. With Just My Code off, VS also decompiles the submission to C# and steps through that; turning off `debugging.general.enableAutomaticDecompilation` removes the C# but does not load the symbols, and setting `debugging.symbols.load.moduleFilterMode` to `loadAllButExcluded` changes nothing. The inference is that Just My Code forces VS to read an embedded PDB, to decide what is user code, and that nothing else does. VS's own reason has not been read, so the file stays.

Attach with the CoreCLR engine. `Process.Attach()` or an automatic engine choice can leave `Debugger.IsAttached` false, and then the guard silently skips the break.

Not verified:

- The .NET Framework run of the changed tests (only net11.0 was run).

## Next steps

1. **Frame names.** VS shows `<Unknown function>` for a submission's `main@`, and inside a function of a later submission it shows `<StartupCode$FSI_0001>.$FSI_0001.main@`, the frame name of a different submission. Line mapping is right, so symbols are found by module while names are resolved against another assembly of the same name. Try a name unique to each submission; `InternalsVisibleTo("FSI-ASSEMBLY-MULTI")` is what ties submissions to one name today.
2. **Watch and Locals** ([#14572](https://github.com/dotnet/fsharp/issues/14572)). With a fixed build of the out-of-tree `FSharp.ExpressionEvaluator` extension, Locals shows a submission's top-level values, and the Immediate window evaluates them and expressions such as `[1;2;3] |> List.rev`. A function from the same submission is not resolved: `sq 2` reports FS0039. Without that fix, the extension's warm-up failed with `ImportILTypeRef: unexpected local scope`, and VS terminated once on an `InvalidCastException` in Roslyn's `CompilationExtensions.GetType`. Without the extension, evaluating a top-level `let` reported "Internal error in the F# compiler" in a clean hive.
3. **The new VS window (#20565).**
   - "Debug in Interactive" still routes to the legacy window's `Hooks.OnMLSend … DebugSelection`, which that PR no longer registers. Implement it in the new window: read `processId` from the handshake, attach with `Process2.Attach2` and the CoreCLR engine, then send `debug = true`.
   - Carry the source origin with each submission instead of the single pending slot.
4. **Legacy window attach.** It uses `Process.Attach()` with no engine specified; use `Attach2` with the right engine.
5. **Console stdin source.** Needs a text-to-line mapping that survives the line resets above, possibly from `FsiStdinSyphon`.
6. **`--multiemit` stays** ([#17308](https://github.com/dotnet/fsharp/issues/17308)). `--multiemit-` is a real workaround for serializers and for hosts such as Rhino. It simply does not get per-submission debug settings.
