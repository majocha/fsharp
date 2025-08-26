namespace FSharp.Compiler.Caches

open System
open System.Collections.Generic
open System.Diagnostics.Metrics

[<Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type internal CacheOptions =
    {
        /// Total capacity, determines the size of the underlying store.
        TotalCapacity: int

        /// Safety margin size as a percentage of TotalCapacity.
        HeadroomPercentage: int
    }

    static member Default: CacheOptions

module internal CacheMetrics =
    val Meter: Meter

[<Class>]
type internal CacheMetricsListener =
    member GetStats: unit -> Map<string, float>
    member GetTotals: unit -> Map<string, int64>

module internal Cache =
    val OverrideCapacityForTesting: unit -> unit

[<Sealed; NoComparison; NoEquality>]
type internal Cache<'Key, 'Value when 'Key: not null> =
    member TryGetValue: key: 'Key * value: outref<'Value> -> bool
    member TryAdd: key: 'Key * value: 'Value -> bool
    member GetOrAdd: key: 'Key * valueFactory: ('Key -> 'Value) -> 'Value
    member AddOrUpdate: key: 'Key * value: 'Value -> unit
    /// Cancels the background eviction task.
    member Dispose: unit -> unit

    interface IDisposable

    /// For testing only
    member Evicted: IEvent<unit>
    member EvictionFailed: IEvent<unit>

    member Metrics: CacheMetricsListener

    static member Create<'Key, 'Value> :
        options: CacheOptions *
        ?comparer: IEqualityComparer<'Key> *
        ?name: string *
        ?observeMetrics: bool *
        ?noEviction: bool ->
            Cache<'Key, 'Value>

