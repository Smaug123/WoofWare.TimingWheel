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
        let maxValueRepresentable : Span = System.Int64.MaxValue

    let toSpanSinceEpoch (t : TimeNs) : Span = t
    let ofSpanSinceEpoch (t : TimeNs) : Span = t
    let toInt64NsSinceEpoch (t : TimeNs) = toSpanSinceEpoch t |> Span.toInt64Ns
    let ofInt64NsSinceEpoch (i : int64) : TimeNs = ofSpanSinceEpoch (Span.ofInt64Ns i)

    let maxValueRepresentable = ofSpanSinceEpoch Span.maxValueRepresentable

    let epoch : TimeNs = Span.zero
