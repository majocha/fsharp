namespace FSharp.Compiler.Caches

open System
open System.Collections.Generic
open System.Diagnostics.Metrics

module internal CacheMetrics =
    val Meter: Meter

[<Class>]
type internal CacheMetricsListener =
    member GetStats: unit -> Map<string, float>
    member GetTotals: unit -> Map<string, int64>
    interface IDisposable

[<RequireQualifiedAccess; NoComparison>]
type internal EvictionMode =
    | NoEviction
    | Immediate
    | MailboxProcessor

[<Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type internal CacheOptions<'Key> =
    {
        /// Total capacity, determines the size of the underlying store.
        TotalCapacity: int

        /// Safety margin size as a percentage of TotalCapacity.
        HeadroomPercentage: int

        /// Mechanism to use for evicting items from the cache.
        EvictionMode: EvictionMode

        Comparer: IEqualityComparer<'Key>
    }

module internal CacheOptions =
    val getDefault: unit -> CacheOptions<'Key> when 'Key: equality
    val getReferenceIdentity: unit -> CacheOptions<'Key> when 'Key: not struct
    val withNoEviction: CacheOptions<'Key> -> CacheOptions<'Key>

module internal Cache =
    val OverrideCapacityForTesting: unit -> unit

[<Sealed; NoComparison; NoEquality>]
type internal Cache<'Key, 'Value when 'Key: not null> =
    new: options: CacheOptions<'Key> * ?name: string -> Cache<'Key, 'Value>
    member TryGetValue: key: 'Key * value: outref<'Value> -> bool
    member TryAdd: key: 'Key * value: 'Value -> bool
    member GetOrAdd: key: 'Key * valueFactory: ('Key -> 'Value) -> 'Value
    member AddOrUpdate: key: 'Key * value: 'Value -> unit

    interface IDisposable

    /// For testing only
    member Evicted: IEvent<unit>
    member EvictionFailed: IEvent<unit>
    member CreateMetricsListener: unit -> CacheMetricsListener
