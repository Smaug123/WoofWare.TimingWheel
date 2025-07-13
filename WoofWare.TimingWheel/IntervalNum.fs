namespace WoofWare.TimingWheel

/// An index of one of the intervals into which a timing-wheel partitions time.
type IntervalNum = int64

/// An index of one of the intervals into which a timing-wheel partitions time.
[<RequireQualifiedAccess>]
module IntervalNum =

    let zero : IntervalNum = 0L
    let one : IntervalNum = 1L
    let minValue : IntervalNum = System.Int64.MinValue
    let maxValue : IntervalNum = System.Int64.MaxValue
    let ofInt64 (i : int64) : IntervalNum = i
    let toInt64 (i : IntervalNum) : int64 = i
    let ofInt (i : int) : IntervalNum = int64 i
    let toIntThrowing (i : IntervalNum) : int = Checked.int i

    let add (i : IntervalNum) (s : IntervalNumSpan) : IntervalNum = i + s
    let sub (i : IntervalNum) (s : IntervalNumSpan) : IntervalNum = i - s
    let diff (i : IntervalNum) (j : IntervalNum) : IntervalNumSpan = i - j
    let succ (i : IntervalNum) : IntervalNum = i + 1L
    let pred (i : IntervalNum) : IntervalNum = i - 1L

    let min (a : IntervalNum) (b : IntervalNum) : IntervalNum = min (toInt64 a) (toInt64 b) |> ofInt64
