namespace WoofWare.TimingWheel

type NumKeyBits = int64

[<RequireQualifiedAccess>]
module NumKeyBits =

    let minValue : NumKeyBits = 0L
    let maxValue : NumKeyBits = 64L - 1L

    let zero : NumKeyBits = 0L
    let one : NumKeyBits = 1L

    let ofInt (i : int) : NumKeyBits = int64<int> i
    let toInt64 (nkb : NumKeyBits) : int64 = nkb

    let (+) (i : NumKeyBits) (j : NumKeyBits) : NumKeyBits = i + j
    let (-) (i : NumKeyBits) (j : NumKeyBits) : NumKeyBits = i - j

    let pow2 (t : NumKeyBits) : NumKeyBits = t <<< 1

    let invariant (t : NumKeyBits) : unit =
        if t < minValue then
            failwith $"expected NumKeyBits {t} at least min value {minValue}"

        if t > maxValue then
            failwith $"expected NumKeyBits {t} at most max value {maxValue}"
