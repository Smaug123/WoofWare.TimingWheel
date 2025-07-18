namespace WoofWare.TimingWheel

type internal NumKeyBits = int

[<RequireQualifiedAccess>]
module internal NumKeyBits =

    [<Literal>]
    let minValue : NumKeyBits = 0

    [<Literal>]
    let maxValue : NumKeyBits = 64 - 1

    [<Literal>]
    let zero : NumKeyBits = 0

    [<Literal>]
    let one : NumKeyBits = 1

    let ofInt (i : int) : NumKeyBits = i
    let toInt64 (nkb : NumKeyBits) : int64 = int64<int> nkb

    let (+) (i : NumKeyBits) (j : NumKeyBits) : NumKeyBits = i + j
    let (-) (i : NumKeyBits) (j : NumKeyBits) : NumKeyBits = i - j

    let pow2 (t : NumKeyBits) : int64 = 1L <<< t

    let invariant (t : NumKeyBits) : unit =
        if t < minValue then
            failwith $"expected NumKeyBits {t} at least min value {minValue}"

        if t > maxValue then
            failwith $"expected NumKeyBits {t} at most max value {maxValue}"
