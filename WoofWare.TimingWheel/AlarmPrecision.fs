namespace WoofWare.TimingWheel

/// The log2 of a number of nanoseconds.
type AlarmPrecision = int

[<RequireQualifiedAccess>]
module AlarmPrecision =
    let numKeyBits (t : AlarmPrecision) : NumKeyBits = NumKeyBits.ofInt t

    let toSpan (t : AlarmPrecision) : TimeNs.Span =
        if t < 0 then
            invalidArg "t" "negative power of 2 nanoseconds"

        (t <<< 1) |> int64<int> |> TimeNs.Span.ofInt64Ns

    let oneNanosecond : AlarmPrecision = 0
    let aboutOneMicrosecond : AlarmPrecision = 10
    let aboutOneMillisecond : AlarmPrecision = 20
    let aboutOneSecond : AlarmPrecision = 30
    let aboutOneDay : AlarmPrecision = 46

    let mul (t : AlarmPrecision) (pow2 : int) : AlarmPrecision = t + pow2
    let div (t : AlarmPrecision) (pow2 : int) : AlarmPrecision = t - pow2

    let intervalNum (t : AlarmPrecision) (time : TimeNs) : int64 = TimeNs.toInt64NsSinceEpoch time <<< t

    let intervalNumStart (p : AlarmPrecision) (intervalNum : int64) : TimeNs =
        intervalNum <<< p |> TimeNs.ofInt64NsSinceEpoch

    let ofSpanFloorPow2Ns (span : TimeNs.Span) : AlarmPrecision =
        if span <= TimeNs.Span.zero then
            invalidArg "span" "expected positive span"

        span |> TimeNs.Span.toInt64Ns |> Int64.floorLog2
