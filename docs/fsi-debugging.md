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
| A unique identity | A random MVID for each module (`GetModuleAsRow` in `ilwrite.fs`) | Debuggers tell modules apart by MVID, and every submission has the same assembly name. See below. |
| Symbols | Portable PDB per submission | On .NET it is embedded in the assembly and nothing goes to disk; stack traces and Visual Studio both read it from the loaded image. .NET Framework passes the bytes to `Assembly.Load` and writes the file to the session's scripting-symbols temp directory. |
| Source | Document on disk, or source embedded in the PDB | See below. |
| A first stop | `#dbgbreak` / `debug = true` inject `if Debugger.IsAttached then Debugger.Break()` before the first breakable line (`FsiDynamicCompiler.CreateDebuggerBreak`) | The `IsAttached` guard matters: an unguarded `Debugger.Break()` terminates a .NET process with no debugger. |

### Why each submission needs its own MVID

The MVID used to be derived from a timestamp taken once per process and from the module name. That is harmless for fsc, which writes one module per process, but multi-emit writes many modules with one name, so every submission of a session had the same MVID. Debuggers then treated all submissions as one module:

- frames showed `<Unknown function>`, or the `main@` of a different submission;
- a later submission opened the decompiled source of an earlier one;
- an embedded PDB was not loaded with Just My Code off, so breakpoints stayed unbound and VS decompiled the submission to C#;
- expression evaluation failed, and Roslyn's evaluator terminated VS once with an `InvalidCastException` in `CompilationExtensions.GetType`, which finds types by module id.

The same bytes loaded from another session worked, because there the MVID was unique. A PDB file hid part of this, because VS found it by path.

### Source for code with no file

Code typed into an interactive window has no file behind it. When `FsiInteractionProcessor.EvalInteraction` gets a file name that does not exist on disk and debug info is on, it wraps the evaluation in `FsiDynamicCompiler.EmbeddingSource`. While that runs, the PDB writer receives an `InMemorySourceDocument` for that IL document. The writer:

- records the document under `<scripting symbols dir>\<n>\<file name>`, unique per `EvalInteraction` call, because a debugger identifies documents by path and every submission would otherwise claim the same `input.fsx`;
- checksums the text (UTF-8);
- embeds the text.

Diagnostics keep the original file name; only the PDB, and therefore stack traces, show the unique name.

Not covered:

- **Console stdin.** Its lines keep counting across submissions, or reset with `# 1 "stdin"` from the legacy VS window, so one submission's text does not line up with the line numbers in its PDB.
- **A `sourcePath` that does not exist** (an unsaved script). The embedded text is the submission behind the line directive the server prepends, so its code starts at line 2 while the PDB records `startLine`. Embedding the whole unsaved document instead would fix it; left as an edge case.

### Per-submission debugging

`fsi/execute` with `debug = true` calls the internal `FsiEvaluationSession.EvalInteractionNonThrowing(code, scriptFileName, debug, cancellationToken)`. For that call only, `withDebuggableCode` switches `tcConfigB` to `--optimize- --tailcalls- --debug:pdbonly`, restoring the saved fields afterwards, and the interactive state's `debugBreak` is set. The flag is cleared afterwards even when the code had no breakable line, so the break never leaks into a later submission. This replaces the legacy window's session-wide "debug mode" option and the session restart it needs.

### Debug in Interactive (new VS window)

Branch `fsi-debug-in-interactive` merges #20565 into this one. In the new window:

- `FSharpInteractiveCommandFilter` handles `cmdIDDebugSelection` alongside Execute Selection and Execute Line, and hides it while the debugger is attached to another process. The editor's `FsiCommandFilter`, which sent it to the legacy window, is gone.
- `SubmissionOrigins` carries a `debug` flag with each submission's `sourcePath` and `startLine`, so only the submission sent for debugging is debugged.
- Before running a debug submission, `FSharpInteractiveEvaluator` starts the session if needed and attaches to its `processId` (`InteractiveDebugger.attachAsync`: `Process2.Attach2` with the CoreCLR engine; nothing to do when already attached). If attaching fails, the submission is not run.
- `fsi/execute` then gets `debug = true`.

## Status

Branch `fsi-debugging-prototype` on [majocha/fsharp](https://github.com/majocha/fsharp/tree/fsi-debugging-prototype), based on `main` at `fe180deb55`, implements:

- a unique MVID for each submission;
- embedded sources for in-memory documents;
- an embedded PDB on .NET, with no PDB file on disk;
- the `debug` field on `fsi/execute`;
- the guarded break.

Tests:

- `tests/FSharp.Compiler.ComponentTests/Scripting/Interactive.fs`, module `PortablePdb`:
  - PDB round trip of an in-memory document;
  - unique document names, and line numbers from in-memory symbols, through a stack trace;
  - the PDB file is written only on .NET Framework;
  - each submission has its own MVID.
- `tests/FSharp.Compiler.Interactive.Server.Tests/FsiJsonRpcServerTests.fs`, section "Debugging": a debug submission is compiled unoptimized, the next one is not.

Verified by driving Visual Studio 2026 Insiders (18.12) through DTE against a JSON-RPC session with `debug = true`, with Just My Code on and off:

- the guarded break stops in the submission;
- frames show the right names, such as `FSI_0003.sq`;
- code with no file opens from the source embedded in the PDB, on the right lines, and each submission gets its own document;
- code with a `sourcePath` opens the real file, and a breakpoint set there binds and is hit;
- stepping walks the submission's lines, over and into its functions;
- no PDB file is written.

Attach with the CoreCLR engine. `Process.Attach()` or an automatic engine choice can leave `Debugger.IsAttached` false, and then the guard silently skips the break.

Not verified:

- The .NET Framework run of the changed tests (only net11.0 was run).
- Debug in Interactive in the new window. It builds, and `SubmissionOriginsTests` covers the flag, but the F# extension built from `main` does not load in VS 18.12 Insiders: `main` flows Roslyn 5.13 and System.* 10.0.14, the IDE has 5.12 and 10.0.10. `start-vs-VisualFSharpSln.ps1` overrides Roslyn (#20528 extends it to local Roslyn builds); overriding System.* to 10.0.10 as well gets the F# MEF parts to scan, but `FSharpPackage` still fails at `LanguageService.fs` line 387 because the workspace has no `IFSharpWorkspaceService`. Needs a newer VS Insiders.

## Next steps

1. **Watch and Locals** ([#14572](https://github.com/dotnet/fsharp/issues/14572)). With the out-of-tree `FSharp.ExpressionEvaluator` extension, locals of a function and expressions such as `[1;2;3] |> List.rev` evaluate, and neither the extension's assert nor the Roslyn crash has come back. The extension does not yet resolve module-level names outside the current frame: `sq 2`, a value from an earlier submission, or a top-level value from inside a function report FS0039.
2. **Verify Debug in Interactive in VS** on a VS Insiders that loads an extension built from `main`. The new window has nothing for the legacy "Attach/Detach Debugger" commands yet.
3. **Legacy window attach.** It uses `Process.Attach()` with no engine specified; use `Attach2` with the right engine.
4. **Console stdin source.** Needs a text-to-line mapping that survives the line resets above, possibly from `FsiStdinSyphon`.
5. **`--multiemit` stays** ([#17308](https://github.com/dotnet/fsharp/issues/17308)). `--multiemit-` is a real workaround for serializers and for hosts such as Rhino. It simply does not get per-submission debug settings.
