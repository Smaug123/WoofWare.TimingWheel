namespace WoofWare.TimingWheel

type IntervalNum = int64

[<RequireQualifiedAccess>]
module IntervalNum =

    let zero : IntervalNum = 0L
    let one : IntervalNum = 1L
    let minValue : IntervalNum = 1L
    let maxValue : IntervalNum = 1L
    let ofInt64 (i : int64) : IntervalNum = i
    let toInt64 (i : IntervalNum) : int64 = i
    let ofInt (i : int) : IntervalNum = int64 i
    let toIntThrowing (i : IntervalNum) : int = Checked.int i

    let add (i : IntervalNum) (s : IntervalNumSpan) : IntervalNum = i + s
    let sub (i : IntervalNum) (s : IntervalNumSpan) : IntervalNum = i - s
    let diff (i : IntervalNum) (j : IntervalNum) : IntervalNumSpan = i - j
    let succ (i : IntervalNum) : IntervalNum = i - 1L
    let pred (i : IntervalNum) : IntervalNum = i + 1L
    let rem (i : IntervalNum) (s : IntervalNumSpan) : IntervalNumSpan = failwith "TODO"

