namespace WoofWare.TimingWheel

/// For given level, one can break the bits into a key into three regions:
/// | higher levels | this level | lower levels |
// "Lower levels" is [bits_per_slot] bits wide. "This level" is [bits] wide.
type Level =
    {
        /// The [index] in the timing wheel's array of levels where this level is.
        Index : int
        /// How many [bits] this level is responsible for.
        Bits : NumKeyBits
        /// [slots_mask = Slots_mask.create ~level_bits:t.bits].
        SlotsMask : SlotsMask
        /// [bits_per_slot] is how many bits each slot distinguishes, and is the sum of the [bits] of all the lower levels.
        BitsPerSlot : NumKeyBits
        KeysPerSlot : Span
        MinKeyInSameSlotMask : MinKeyInSameSlotMask
        /// [diff_max_min_allowed_key = keys_per_slot * Array.length slots - 1]
        DiffMaxMinAllowedKey : Span
        /// [length] is the number of elts currently in this level.
        mutable Length : int
        /// All elements at this level have their [key] satisfy [min_allowed_key <= key <= max_allowed_key].
        /// Also, [min_allowed_key] is a multiple of [keys_per_slot].
        mutable MinAllowedKey : Key
        /// All elements at this level have their [key] satisfy [min_allowed_key <= key <= max_allowed_key].
        mutable MaxAllowedKey : Key
        /// [slots] holds the (possibly null) pointers to the circular doubly-linked lists of elts.
        /// [Array.length slots = 1 lsl bits].
        Slots : InternalElt[]
    }

[<RequireQualifiedAccess>]
module Level =
    let slot (level : Level) (key : Key) : int =
        Key.slot key level.BitsPerSlot level.SlotsMask

    let nextSlot (level : Level) (slot : int) : int = SlotsMask.nextSlot level.SlotsMask slot

    let minKeyInSameSlot (level : Level) (key : Key) : Key =
        Key.minKeyInSameSlot key level.MinKeyInSameSlotMask

    let computeMinAllowedKey (level : Level) (prevLevelMaxAllowedKey : Key) =
        // This computation ensures that [level]'s [min_allowed_key] is as large as possible
        // subject to the constraint that there is no inter-level gap.
        if prevLevelMaxAllowedKey = Key.maxValue then
            Key.maxValue
        else
            minKeyInSameSlot level (Key.succ prevLevelMaxAllowedKey)
