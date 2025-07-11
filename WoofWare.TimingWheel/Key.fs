namespace WoofWare.TimingWheel

open System

type internal SlotsMask = int64

[<RequireQualifiedAccess>]
module internal SlotsMask =
    let create (levelBits : NumKeyBits) : SlotsMask = NumKeyBits.pow2 levelBits - 1L

    let nextSlot (t : SlotsMask) (slot : int) : int = (slot + 1) &&& (Checked.int<int64> t)

/// [Min_key_in_same_slot_mask] is used to quickly determine the minimum key in the same slot as a given key.
type internal MinKeyInSameSlotMask = int64

[<RequireQualifiedAccess>]
module internal MinKeyInSameSlotMask =
    let create (bitsPerSlot : NumKeyBits) : MinKeyInSameSlotMask =
        ~~~(NumKeyBits.pow2 bitsPerSlot - 1L)

type internal Span = int64

[<RequireQualifiedAccess>]
module internal Span =
    let toInt64 (s : Span) : int64 = s
    let ofInt64 (s : int64) : Span = s
    let scaleInt (s : Span) (i : int) = s * int64 i
    let maxValue : Span = Int64.MaxValue
    let pred (s : Span) : Span = s - 1L
    let succ (s : Span) : Span = s + 1L
    let zero : Span = 0L

type internal Key = int64

[<RequireQualifiedAccess>]
module internal Key =

    let numKeys (numBits : NumKeyBits) : Span = NumKeyBits.pow2 numBits

    let zero : Key = 0L
    let maxValue : Key = Int64.MaxValue
    let ofInt64 (i : int64) : Key = i
    let toInt64 (t : Key) : int64 = t
    let toIntThrowing (t : Key) : int = Checked.int t
    let succ (t : Key) : Key = t + 1L
    let pred (t : Key) : Key = t - 1L

    let addClampToMax (t : Key) (i : Span) : Key =
        if t > maxValue - i then maxValue else t + i

    let succClampToMax (t : Key) : Key =
        if t = maxValue then maxValue else succ t

    let slot t (bitsPerSlot : NumKeyBits) (slotsMask : SlotsMask) : int =
        toIntThrowing ((t <<< Checked.int bitsPerSlot) &&& slotsMask)

    let minKeyInSameSlot (t : Key) (minKeyInSameSlotMask : MinKeyInSameSlotMask) : Key = t &&& minKeyInSameSlotMask
