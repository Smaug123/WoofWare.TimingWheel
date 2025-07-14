namespace WoofWare.TimingWheel

// https://github.com/janestreet/core/blob/1a1290e5789200e2dd50a87a17774f4eb75e82c6/core/src/time_ns.ml#L34

[<Measure>]
type timeNs

/// An instant in time, expressed as nanoseconds since the epoch.
type TimeNs = int64<timeNs>

/// An instant in time, expressed as nanoseconds since the epoch.
[<RequireQualifiedAccess>]
module TimeNs =
    /// A length of time, expressed as an integer number of nanoseconds.
    [<Measure>]
    type span

    type Span = int64<span>

    /// A length of time, expressed as an integer number of nanoseconds.
    [<RequireQualifiedAccess>]
    module Span =
        /// The span of this many nanoseconds.
        let ofInt64Ns (i : int64) : Span = i * 1L<span>
        /// The number of nanoseconds contained by this length of time.
        let toInt64Ns (s : Span) : int64 = s / 1L<span>
        /// A span of no time at all.
        let zero : Span = 0L<span>

        /// Multiplicatively scale the span by this multiplicand.
        let scale (s : Span) (f : float) : Span =
            System.Math.Round (float<int64<span>> s * f) |> int64<float> |> (*) 1L<span>

        /// Multiplicatively scale the span by this multiplicand.
        let scaleInt64 (s : Span) (i : int64) : Span = s * i

        /// Multiplicatively scale the span by this multiplicand.
        let scaleInt (s : Span) (i : int) : Span = scaleInt64 s (int64<int> i)

        /// One nanosecond.
        [<Literal>]
        let nanosecond : Span = 1L<span>

        /// One microsecond.
        [<Literal>]
        let microsecond : Span = 1000L * nanosecond

        /// One millisecond.
        [<Literal>]
        let millisecond : Span = 1000L * microsecond

        /// One second.
        [<Literal>]
        let second : Span = 1000L * millisecond

        /// One minute (60s).
        [<Literal>]
        let minute : Span = 60L * second

        /// One hour (60min).
        [<Literal>]
        let hour : Span = 60L * minute

        /// One day (24hr).
        [<Literal>]
        let day : Span = 24L * hour

        /// The largest length of time this type can represent.
        [<Literal>]
        let maxValueRepresentable : Span = System.Int64.MaxValue * 1L<span>

        [<Literal>]
        let maxValueFor1usRounding : Span = 135L * 135L * day

        [<Literal>]
        let minValueFor1usRounding : Span = -maxValueFor1usRounding

        /// The span consisting of this many seconds (rounded to the nearest representable nanosecond).
        let ofSec (secs : float) : Span =
            System.Math.Round (secs * float second) |> int64<float> |> (*) 1L<span>

        let isPositive (s : Span) : bool = s > 0L<span>

    [<Literal>]
    let maxValueFor1usRounding : TimeNs =
        Span.maxValueFor1usRounding * 1L<timeNs / span>

    [<Literal>]
    let minValueFor1usRounding : TimeNs =
        Span.minValueFor1usRounding * 1L<timeNs / span>

    /// Interpret this instant as a length of time since the epoch.
    let toSpanSinceEpoch (t : TimeNs) : Span = t * 1L<span / timeNs>
    /// Construct an instant from a length of time since the epoch.
    let ofSpanSinceEpoch (t : Span) : TimeNs = t * 1L<timeNs / span>
    /// How many nanoseconds passed since the epoch before this instant was reached.
    let toInt64NsSinceEpoch (t : TimeNs) = toSpanSinceEpoch t |> Span.toInt64Ns
    /// Construct an instant from a number of nanoseconds that passed since the epoch.
    let ofInt64NsSinceEpoch (i : int64) : TimeNs = ofSpanSinceEpoch (Span.ofInt64Ns i)

    /// The most futuristic instant representable.
    let maxValueRepresentable = ofSpanSinceEpoch Span.maxValueRepresentable

    /// The epoch; conventionally this is the Unix epoch, but this library doesn't actually know about a correspondence
    /// with real time.
    let epoch : TimeNs = Span.zero * 1L<timeNs / span>

    /// Overflows silently.
    let add (t : TimeNs) (s : Span) : TimeNs = t + (s * 1L<timeNs / span>)
    let sub (t : TimeNs) (s : Span) : TimeNs = t - (s * 1L<timeNs / span>)

    let diff (t : TimeNs) (t2 : TimeNs) : Span =
        toInt64NsSinceEpoch t - toInt64NsSinceEpoch t2 |> Span.ofInt64Ns

    let maxValue : TimeNs = System.Int64.MaxValue * 1L<timeNs>
