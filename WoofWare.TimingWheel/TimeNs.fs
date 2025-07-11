namespace WoofWare.TimingWheel

// https://github.com/janestreet/core/blob/1a1290e5789200e2dd50a87a17774f4eb75e82c6/core/src/time_ns.ml#L34

type TimeNs = int64

[<RequireQualifiedAccess>]
module TimeNs =
    type Span = int64

    [<RequireQualifiedAccess>]
    module Span =
        let ofInt64Ns (i : int64) : Span = i
        let toInt64Ns (s : Span) : int64 = s
        let zero : Span = 0L

        [<Literal>]
        let nanosecond : Span = 1L

        [<Literal>]
        let microsecond : Span = 1000L * nanosecond

        [<Literal>]
        let millisecond : Span = 1000L * microsecond

        [<Literal>]
        let second : Span = 1000L * millisecond

        [<Literal>]
        let minute : Span = 60L * second

        [<Literal>]
        let hour : Span = 60L * minute

        [<Literal>]
        let day : Span = 24L * hour

        [<Literal>]
        let maxValueRepresentable : Span = System.Int64.MaxValue

        [<Literal>]
        let maxValueFor1usRounding : Span = 135L * 135L * day

        [<Literal>]
        let minValueFor1usRounding : Span = -maxValueFor1usRounding


    [<Literal>]
    let maxValueFor1usRounding : TimeNs = Span.maxValueFor1usRounding

    [<Literal>]
    let minValueFor1usRounding : TimeNs = Span.minValueFor1usRounding

    let toSpanSinceEpoch (t : TimeNs) : Span = t
    let ofSpanSinceEpoch (t : Span) : TimeNs = t
    let toInt64NsSinceEpoch (t : TimeNs) = toSpanSinceEpoch t |> Span.toInt64Ns
    let ofInt64NsSinceEpoch (i : int64) : TimeNs = ofSpanSinceEpoch (Span.ofInt64Ns i)

    let maxValueRepresentable = ofSpanSinceEpoch Span.maxValueRepresentable

    let epoch : TimeNs = Span.zero

    /// overflows silently
    let add (t : TimeNs) (s : Span) : TimeNs = t + s
