namespace WoofWare.TimingWheel

// https://github.com/janestreet/core/blob/1a1290e5789200e2dd50a87a17774f4eb75e82c6/core/src/time_ns.ml#L34

/// An instant in time, expressed as nanoseconds since the epoch.
type TimeNs = int64

/// An instant in time, expressed as nanoseconds since the epoch.
[<RequireQualifiedAccess>]
module TimeNs =
    /// A length of time, expressed as an integer number of nanoseconds.
    type Span = int64

    /// A length of time, expressed as an integer number of nanoseconds.
    [<RequireQualifiedAccess>]
    module Span =
        /// The span of this many nanoseconds.
        let ofInt64Ns (i : int64) : Span = i
        /// The number of nanoseconds contained by this length of time.
        let toInt64Ns (s : Span) : int64 = s
        /// A span of no time at all.
        let zero : Span = 0L

        /// Multiplicatively scale the span by this multiplicand.
        let scale (s : Span) (f : float) : Span =
            System.Math.Round (float<int64> s * f) |> int64<float>

        /// Multiplicatively scale the span by this multiplicand.
        let scaleInt64 (s : Span) (i : int64) : Span = s * i

        /// Multiplicatively scale the span by this multiplicand.
        let scaleInt (s : Span) (i : int) : Span = scaleInt64 s (int64<int> i)

        /// One nanosecond.
        [<Literal>]
        let nanosecond : Span = 1L

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
        let maxValueRepresentable : Span = System.Int64.MaxValue

        [<Literal>]
        let maxValueFor1usRounding : Span = 135L * 135L * day

        [<Literal>]
        let minValueFor1usRounding : Span = -maxValueFor1usRounding

        /// The span consisting of this many seconds (rounded to the nearest representable nanosecond).
        let ofSec (secs : float) : Span =
            System.Math.Round (secs * float second) |> int64

    [<Literal>]
    let maxValueFor1usRounding : TimeNs = Span.maxValueFor1usRounding

    [<Literal>]
    let minValueFor1usRounding : TimeNs = Span.minValueFor1usRounding

    /// Interpret this instant as a length of time since the epoch.
    let toSpanSinceEpoch (t : TimeNs) : Span = t
    /// Construct an instant from a length of time since the epoch.
    let ofSpanSinceEpoch (t : Span) : TimeNs = t
    /// How many nanoseconds passed since the epoch before this instant was reached.
    let toInt64NsSinceEpoch (t : TimeNs) = toSpanSinceEpoch t |> Span.toInt64Ns
    /// Construct an instant from a number of nanoseconds that passed since the epoch.
    let ofInt64NsSinceEpoch (i : int64) : TimeNs = ofSpanSinceEpoch (Span.ofInt64Ns i)

    /// The most futuristic instant representable.
    let maxValueRepresentable = ofSpanSinceEpoch Span.maxValueRepresentable

    /// The epoch; conventionally this is the Unix epoch, but this library doesn't actually know about a correspondence
    /// with real time.
    let epoch : TimeNs = Span.zero

    /// Overflows silently.
    let add (t : TimeNs) (s : Span) : TimeNs = t + s
