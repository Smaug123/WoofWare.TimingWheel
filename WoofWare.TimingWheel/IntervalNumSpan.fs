namespace WoofWare.TimingWheel

type IntervalNumSpan = int64

[<RequireQualifiedAccess>]
module IntervalNumSpan =
    let ofInt64 (i : int64) : IntervalNumSpan = i
    let toInt64 (i : IntervalNumSpan) : int64 = i

    let ofInt (i : int) : IntervalNumSpan = int64 i
    let toIntThrowing (i : IntervalNumSpan) : int = Checked.int i

    let scaleInt (t : IntervalNumSpan) (i : int) : IntervalNumSpan = t * ofInt i
    let pred (i : IntervalNumSpan) : IntervalNumSpan = i - 1L
    let succ (i : IntervalNumSpan) : IntervalNumSpan = i + 1L

    let zero : IntervalNumSpan = 0L
    let one : IntervalNumSpan = 1L
    let maxValue : IntervalNumSpan = System.Int64.MaxValue

