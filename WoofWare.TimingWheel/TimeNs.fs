namespace WoofWare.TimingWheel

// https://github.com/janestreet/core/blob/1a1290e5789200e2dd50a87a17774f4eb75e82c6/core/src/time_ns.ml#L34

/// The tag indicating that a number represents an absolute instant in time since some epoch.
[<Measure>]
type timeNs

/// An instant in time, expressed as nanoseconds since the epoch.
type TimeNs = int64<timeNs>

/// An instant in time, expressed as nanoseconds since the epoch.
[<RequireQualifiedAccess>]
module TimeNs =
    /// The tag indicating that a number represents a length of time.
    [<Measure>]
    type span

    /// A length of time, expressed as an integer number of nanoseconds.
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

        let div (a : Span) (b : Span) : int64 = a / b

        /// A somewhat imprecise ToString function; this rounds to the nearest time
        /// representable in .NET before displaying.
        let display (t : Span) : string =
            let ts = System.TimeSpan.FromMilliseconds (float t / 1_000_000.0)
            let seconds = $"%02d{ts.Seconds}s.%09.0f{(ts.TotalSeconds % 1.0) * 1_000_000_000.0}"

            let minutes =
                if ts.TotalMinutes >= 1.0 then
                    $"%02d{ts.Minutes}m%s{seconds}"
                else
                    seconds

            let hours =
                if ts.TotalHours >= 1.0 then
                    $"%02d{ts.Hours}h%s{minutes}"
                else
                    minutes

            if ts.TotalDays >= 1.0 then
                $"%02d{ts.Days}d%s{hours}"
            else
                hours

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

    let private nextMultipleInternal (name : string) (canEqualAfter : bool) base_ after interval =
        if interval <= Span.zero then
            failwith $"%s{name} got nonpositive interval"

        let baseToAfter = diff after base_

        if baseToAfter < Span.zero then
            // after < base, so choose k = 0
            base_
        else
            let next = add base_ (Span.scaleInt64 interval (Span.div baseToAfter interval))

            if next > after || (canEqualAfter && next = after) then
                next
            else
                add next interval

    /// Default `canEqualAfter` is `false`.
    let nextMultiple (canEqualAfter : bool option) (base_ : TimeNs) (after : TimeNs) (interval : Span) : TimeNs =
        let canEqualAfter = defaultArg canEqualAfter false
        nextMultipleInternal "nextMultiple" canEqualAfter base_ after interval

    /// A slightly imprecise display of this instant, interpreting `epoch` as the Unix epoch.
    /// This is imprecise because it's rounded to the nearest time representable in .NET before display.
    let display (t : TimeNs) : string =
        let ts = System.TimeSpan.FromMilliseconds (float<int64<_>> t / 1_000_000.0)
        let dt = System.DateTime.UnixEpoch + ts
        dt.ToString "O"
