namespace WoofWare.TimingWheel

type internal PriorityQueue<'a> =
    private
        {
            mutable Length : int
            mutable Pool : Pool<'a>
            mutable MinElt : InternalElt
            mutable EltKeyLowerBound : Key
            Levels : Level[]
        }

type internal Elt = ExternalElt

[<RequireQualifiedAccess>]
module internal Elt =
    let null' = ExternalElt.None

    let at (p : PriorityQueue<ExternalEltValue<'a>>) (t : ExternalElt) : TimeNs =
        InternalElt.atTime p.Pool (InternalElt.ofExternalThrowing p.Pool t)

    let key (p : PriorityQueue<ExternalEltValue<'a>>) (t : ExternalElt) : Key =
        InternalElt.key p.Pool (InternalElt.ofExternalThrowing p.Pool t)

    let value (p : PriorityQueue<ExternalEltValue<'a>>) (t : ExternalElt) : 'a =
        InternalElt.value p.Pool (InternalElt.ofExternalThrowing p.Pool t)

[<RequireQualifiedAccess>]
module internal PriorityQueue =
    let isEmpty (t : PriorityQueue<'a>) : bool = t.Length = 0
    let numLevels (t : PriorityQueue<'a>) : int = Array.length t.Levels
    let minAllowedKey (t : PriorityQueue<'a>) : Key = t.Levels.[0].MinAllowedKey

    let maxAllowedKey (t : PriorityQueue<'a>) : Key =
        t.Levels.[numLevels t - 1].MaxAllowedKey

    let internal internalIter (t : PriorityQueue<ExternalEltValue<'a>>) (f : InternalElt -> unit) : unit =
        if t.Length = 0 then
            ()
        else

        let pool = t.Pool
        let levels = t.Levels

        for levelIndex = 0 to Array.length levels - 1 do
            let level = levels.[levelIndex] in

            if level.Length > 0 then
                let slots = level.Slots

                for slotIndex = 0 to Array.length slots - 1 do
                    let elt = slots.[slotIndex]

                    if not (InternalElt.isNull elt) then
                        InternalElt.iter pool elt f


    let iter (t : PriorityQueue<ExternalEltValue<'a>>) (f : Elt -> unit) : unit = internalIter t f

    let computeDiffMaxMinAllowedKey (levelBits : NumKeyBits) (bitsPerSlot : NumKeyBits) =
        let bits = levelBits + bitsPerSlot in

        if bits = NumKeyBits.MaxValue then
            Span.maxValue
        else
            Span.pred (Key.numKeys bits)

    let invariant (inv : 'a -> unit) (t : PriorityQueue<ExternalEltValue<'a>>) : unit =
        let pool = t.Pool

        let levelInvariant (level : Level) : unit =
            if level.Index < 0 then
                failwith "expected nonnegative level"

            if level.Bits <= NumKeyBits.zero then
                failwith "expected strictly positive number of bits"

            if level.SlotsMask <> SlotsMask.create level.Bits then
                failwith "unexpected slots mask"

            if level.BitsPerSlot < NumKeyBits.zero then
                failwith "expected nonnegative bitsPerSlot"

            if level.KeysPerSlot <> Key.numKeys level.BitsPerSlot then
                failwith "unexpected keysPerSlot"

            if level.MinKeyInSameSlotMask <> MinKeyInSameSlotMask.create level.BitsPerSlot then
                failwith "unexpected minKeyInSameSlotMask"

            if
                level.DiffMaxMinAllowedKey
                <> computeDiffMaxMinAllowedKey level.Bits level.BitsPerSlot
            then
                failwith "unexpected DiffMinMaxAllowedKey"

            do
                let expectedLength =
                    (0, level.Slots)
                    ||> Array.fold (fun n elt ->
                        if InternalElt.isNull elt then
                            n
                        else
                            n + InternalElt.length pool elt
                    )

                if level.Length <> expectedLength then
                    failwith "unexpected length"

            do
                if level.MinAllowedKey < Key.zero then
                    failwith "expected nonnegative MinAllowedKey"

                if level.MinAllowedKey < Key.maxValue then
                    if level.MinAllowedKey % level.KeysPerSlot <> Span.zero then
                        failwith "TODO: describe failure mode"

            if
                level.MaxAllowedKey
                <> Key.addClampToMax level.MinAllowedKey level.DiffMaxMinAllowedKey
            then
                failwith "unexpected MaxAllowedKey"

            do
                for elt in level.Slots do
                    if not (InternalElt.isNull elt) then
                        InternalElt.invariant pool inv elt

                        InternalElt.iter
                            pool
                            elt
                            (fun elt ->
                                if InternalElt.key pool elt < level.MinAllowedKey then
                                    failwith "got key less than min"

                                if InternalElt.key pool elt > level.MaxAllowedKey then
                                    failwith "got key bigger than max"

                                if InternalElt.levelIndex pool elt <> level.Index then
                                    failwith "unexpected level index"

                                inv (InternalElt.value pool elt)
                            )

        if minAllowedKey t < Key.zero then
            failwith "expected nonnegative key"

        if maxAllowedKey t < minAllowedKey t then
            failwith "expected max geq min"

        if t.Length < 0 then
            failwith "unexpected negative length"

        if not (InternalElt.isNull t.MinElt) then
            if not (InternalElt.isValid t.Pool t.MinElt) then
                failwith "got invalid MinElt"

            if t.EltKeyLowerBound <> InternalElt.key t.Pool t.MinElt then
                failwith "expected min elt to attain key lower bound"

        do
            if t.EltKeyLowerBound < minAllowedKey t then
                failwith "expected EltKeyLowerBound at least MinAllowedKey"

            if t.EltKeyLowerBound > maxAllowedKey t then
                failwith "expected EltKeyLowerBound at most MaxAllowedKey"

            if not (InternalElt.isNull t.MinElt) then
                if t.EltKeyLowerBound <> InternalElt.key t.Pool t.MinElt then
                    failwith "expected min elt to attain key lower bound"

        if numLevels t <= 0 then
            failwith "expected at least one level"

        t.Levels
        |> Array.iteri (fun levelIndex level ->
            if levelIndex <> level.Index then
                failwith $"expected level %i{levelIndex} to have that index"
                levelInvariant level

                if levelIndex > 0 then
                    let prevLevel = t.Levels.[levelIndex - 1]

                    if level.KeysPerSlot <> Span.succ prevLevel.DiffMaxMinAllowedKey then
                        failwith "TODO: describe failure mode"

                    if level.MinAllowedKey <> Level.computeMinAllowedKey level prevLevel.MaxAllowedKey then
                        failwith "TODO: describe failure mode"
        )

    let minElt' (t : PriorityQueue<ExternalEltValue<'a>>) : InternalElt =
        if isEmpty t then
            InternalElt.null'
        elif not (InternalElt.isNull t.MinElt) then
            t.MinElt
        else

        let pool = t.Pool
        let mutable minEltAlreadyFound = InternalElt.null'
        let mutable minKeyAlreadyFound = Key.maxValue
        let mutable levelIndex = 0
        let numLevels = numLevels t

        while levelIndex < numLevels do
            let level = t.Levels.[levelIndex]

            if level.MinAllowedKey > minKeyAlreadyFound then
                //  We don't need to consider any more levels.  Quit the loop.
                levelIndex <- numLevels
            elif level.Length = 0 then
                levelIndex <- levelIndex + 1
            else
            // Look in `level`.
            let slots = level.Slots

            let mutable slotMinKey =
                Level.minKeyInSameSlot level (max level.MinAllowedKey t.EltKeyLowerBound)

            let mutable slot = Level.slot level slotMinKey
            // Find the first nonempty slot with a small enough slotMinKey.
            while InternalElt.isNull slots.[slot] && slotMinKey < minKeyAlreadyFound do
                slot <- Level.nextSlot level slot
                slotMinKey <- slotMinKey + level.KeysPerSlot

            let first = slots.[slot]

            if not (InternalElt.isNull first) then
                let mutable cont = true
                let mutable current = first

                while cont do
                    let currentKey = InternalElt.key pool current

                    if currentKey <= minKeyAlreadyFound then
                        minEltAlreadyFound <- current
                        minKeyAlreadyFound <- currentKey

                    let next = InternalElt.next pool current
                    // If [!level_index = 0] then all elts in this slot have the same [key],
                    // i.e. [!slot_min_key].  So, we don't have to check any elements after
                    // [first].  This is a useful shortcut in the common case that there are
                    // multiple elements in the same min slot in level 0.
                    if next = first || levelIndex = 0 then
                        cont <- false
                    else
                        current <- next

            // Finished looking in [level].  Move up to the next level.
            levelIndex <- levelIndex + 1

        t.MinElt <- minEltAlreadyFound
        t.EltKeyLowerBound <- minKeyAlreadyFound
        t.MinElt

    let internal raiseAddEltKeyOutOfBounds (key : Key) (t : PriorityQueue<'a>) : unit =
        failwith $"PriorityQueue.addElt key {key} out of bounds ({minAllowedKey t} .. {maxAllowedKey t})"

    let internal raiseAddEltKeyOutOfLevelBounds (key : Key) (level : Level) =
        failwith $"PriorityQueue.addElt key {key} out of level bounds: {level}"

    let addElt (t : PriorityQueue<ExternalEltValue<'a>>) elt =
        let pool = t.Pool
        let key = InternalElt.key pool elt

        if not (key >= minAllowedKey t && key <= maxAllowedKey t) then
            raiseAddEltKeyOutOfBounds key t

        // Find the lowest level that will hold [elt].
        let levelIndex =
            let mutable levelIndex = 0

            while key > t.Levels.[levelIndex].MaxAllowedKey do
                levelIndex <- levelIndex + 1

            levelIndex

        let level = t.Levels.[levelIndex]

        if not (key >= level.MinAllowedKey && key <= level.MaxAllowedKey) then
            raiseAddEltKeyOutOfLevelBounds key level

        level.Length <- level.Length + 1
        InternalElt.setLevelIndex pool elt levelIndex
        let slot = Level.slot level key
        let slots = level.Slots
        let first = slots.[slot]

        if not (InternalElt.isNull first) then
            InternalElt.insertAtEnd pool first elt
        else
            slots.[slot] <- elt
            InternalElt.linkToSelf pool elt

    let internalAddElt (t : PriorityQueue<ExternalEltValue<'a>>) (elt : InternalElt) : unit =
        let key = InternalElt.key t.Pool elt in

        if key < t.EltKeyLowerBound then
            t.MinElt <- elt
            t.EltKeyLowerBound <- key

        addElt t elt
        t.Length <- t.Length + 1

    let internal raiseGotInvalidKey (t : PriorityQueue<'a>) (key : Key) : unit =
        failwith $"addAtIntervalNum got invalid interval num {key} ({minAllowedKey t} .. {maxAllowedKey t})"

    let internal ensureValidKey (t : PriorityQueue<'a>) (key : Key) : unit =
        if key < minAllowedKey t || key > maxAllowedKey t then
            raiseGotInvalidKey t key

    let internalAdd (t : PriorityQueue<ExternalEltValue<'a>>) (key : Key) (atTime : TimeNs) (value : 'a) : InternalElt =
        ensureValidKey t key

        if Pool.isFull t.Pool then
            Pool.grow None t.Pool

        let elt = InternalElt.create t.Pool key atTime value -1
        internalAddElt t elt
        elt

    /// [remove_or_re_add_elts] visits each element in the circular doubly-linked list
    ///  [first]. If the element's key is [>= t_min_allowed_key], then it adds the element
    ///  back at a lower level. If not, then it calls [handle_removed] and [free]s the
    ///  element.
    let removeOrReAddElts
        (t : PriorityQueue<ExternalEltValue<'a>>)
        (level : Level)
        (first : InternalElt)
        (tMinAllowedKey : Key)
        (handleRemoved : ExternalElt -> unit)
        : unit
        =
        let pool = t.Pool
        let mutable current = first
        let mutable cont = true

        while cont do
            // We extract [next] from [current] first, because we will modify or [free]
            // [current] before continuing the loop.
            let next = InternalElt.next pool current
            level.Length <- level.Length - 1

            if (InternalElt.key pool current) >= tMinAllowedKey then
                addElt t current
            else
                t.Length <- t.Length - 1
                handleRemoved (InternalElt.toExternal current)
                InternalElt.free pool current

            if next = first then cont <- false else current <- next

    /// [increase_level_min_allowed_key] increases the [min_allowed_key] of [level] to as
    ///   large a value as possible, but no more than [max_level_min_allowed_key].
    ///   [t_min_allowed_key] is the minimum allowed key for the entire timing wheel. As
    ///   elements are encountered, they are removed from the timing wheel if their key is
    ///   smaller than [t_min_allowed_key], or added at a lower level if not.
    let increaseLevelMinAllowedKey
        (t : PriorityQueue<ExternalEltValue<'a>>)
        (level : Level)
        (prevLevelMaxAllowedKey : Key)
        (tMinAllowedKey : Key)
        (handleRemoved : ExternalElt -> unit)
        : unit
        =
        let desiredMinAllowedKey = Level.computeMinAllowedKey level prevLevelMaxAllowedKey
        // We require that [mod level.min_allowed_key level.keys_per_slot = 0].  So,
        //   we start [level_min_allowed_key] where that is true, and then increase it by
        //   [keys_per_slot] each iteration of the loop.
        let levelMinAllowedKey =
            Level.minKeyInSameSlot level (min desiredMinAllowedKey (max level.MinAllowedKey t.EltKeyLowerBound))

        let mutable levelMinAllowedKey = levelMinAllowedKey
        let mutable slot = Level.slot level levelMinAllowedKey
        let keysPerSlot = level.KeysPerSlot
        let slots = level.Slots

        while levelMinAllowedKey < desiredMinAllowedKey do
            if level.Length = 0 then
                // If no elements remain at this level, we can just set [min_allowed_key] to the desired value.
                levelMinAllowedKey <- desiredMinAllowedKey
            else
                let first = slots.[slot] in

                if not (InternalElt.isNull first) then
                    slots.[slot] <- InternalElt.null'
                    removeOrReAddElts t level first tMinAllowedKey handleRemoved

                slot <- Level.nextSlot level slot
                levelMinAllowedKey <- Key.addClampToMax levelMinAllowedKey keysPerSlot

        level.MinAllowedKey <- desiredMinAllowedKey
        level.MaxAllowedKey <- Key.addClampToMax desiredMinAllowedKey level.DiffMaxMinAllowedKey

    type IncreaseMinAllowedKeyResult =
        | MaxAllowedKeyDidNotChange
        | MaxAllowedKeyMaybeChanged

    let increaseMinAllowedKey t key handleRemoved : IncreaseMinAllowedKeyResult =
        if key <= (minAllowedKey t) then
            IncreaseMinAllowedKeyResult.MaxAllowedKeyDidNotChange
        else
            // We increase the [min_allowed_key] of levels in order to restore the invariant
            // that they have as large as possible a [min_allowed_key], while leaving no gaps
            // in keys.
            let mutable levelIndex = 0
            let mutable result = IncreaseMinAllowedKeyResult.MaxAllowedKeyMaybeChanged
            let mutable prevLevelMaxAllowedKey = Key.pred key
            let levels = t.Levels
            let numLevels = numLevels t

            while levelIndex < numLevels do
                let level = levels.[levelIndex]
                let minAllowedKeyBefore = level.MinAllowedKey
                increaseLevelMinAllowedKey t level prevLevelMaxAllowedKey key handleRemoved

                if level.MinAllowedKey = minAllowedKeyBefore then
                    // This level did not shift.  Don't shift any higher levels.
                    levelIndex <- numLevels
                    result <- IncreaseMinAllowedKeyResult.MaxAllowedKeyDidNotChange
                else
                    // Level [level_index] shifted.  Consider shifting higher levels.
                    levelIndex <- levelIndex + 1
                    prevLevelMaxAllowedKey <- level.MaxAllowedKey

            if key > t.EltKeyLowerBound then
                // We have removed [t.min_elt] or it was already null, so just set it to null.
                t.MinElt <- InternalElt.null'
                t.EltKeyLowerBound <- minAllowedKey t

            result

    let create capacity (levelBits : LevelBits option) =
        let levelBits =
            match levelBits with
            | None -> LevelBits.default'
            | Some l -> l

        let _, _, _, levels =
            ((0, NumKeyBits.zero, Key.zero, []), levelBits)
            ||> List.fold (fun (index, bitsPerSlot, maxLevelMinAllowedKey, levels) (levelBits : NumKeyBits) ->
                let keysPerSlot = Key.numKeys bitsPerSlot
                let diffMaxMinAllowedKey = computeDiffMaxMinAllowedKey levelBits bitsPerSlot
                let minKeyInSameSlotMask = MinKeyInSameSlotMask.create bitsPerSlot
                let minAllowedKey = Key.minKeyInSameSlot maxLevelMinAllowedKey minKeyInSameSlotMask
                let maxAllowedKey = Key.addClampToMax minAllowedKey diffMaxMinAllowedKey

                let slots =
                    Array.replicate (Checked.int<int64> (NumKeyBits.pow2 levelBits)) InternalElt.null'

                let level : Level =
                    {
                        Index = index
                        Bits = levelBits
                        SlotsMask = SlotsMask.create levelBits
                        BitsPerSlot = bitsPerSlot
                        KeysPerSlot = keysPerSlot
                        MinKeyInSameSlotMask = minKeyInSameSlotMask
                        DiffMaxMinAllowedKey = diffMaxMinAllowedKey
                        Length = 0
                        MinAllowedKey = minAllowedKey
                        MaxAllowedKey = maxAllowedKey
                        Slots = slots
                    }

                (index + 1, levelBits + bitsPerSlot, Key.succClampToMax maxAllowedKey, level :: levels)
            )

        let pool =
            Pool.create<ExternalEltValue<'a>> None (fun i arr v -> arr.[i] <- v) capacity

        let levels = Array.ofList (List.rev levels)

        {
            Length = 0
            Pool = pool
            MinElt = InternalElt.null'
            EltKeyLowerBound = Key.zero
            Levels = levels
        }

    let mem (t : PriorityQueue<'a>) (elt : ExternalElt) : bool = InternalElt.externalIsValid t.Pool elt

    let internalRemove (t : PriorityQueue<ExternalEltValue<'a>>) (elt : InternalElt) : unit =
        let pool = t.Pool

        if elt = t.MinElt then
            t.MinElt <- InternalElt.null'
        // We keep [t.elt_lower_bound] since it is valid even though [t.min_elt] is being removed.
        t.Length <- t.Length - 1
        let level = t.Levels.[InternalElt.levelIndex pool elt]
        level.Length <- level.Length - 1
        let slots = level.Slots
        let slot = Level.slot level (InternalElt.key pool elt)
        let first = slots.[slot]

        if elt = InternalElt.next pool elt then
            // [elt] is the only element in the slot
            slots.[slot] <- InternalElt.null'
        else
            if elt = first then
                slots.[slot] <- InternalElt.next pool elt

            InternalElt.unlink pool elt

    let remove (t : PriorityQueue<ExternalEltValue<'a>>) (elt : ExternalElt) : unit =
        let pool = t.Pool
        let elt = InternalElt.ofExternalThrowing pool elt
        internalRemove t elt
        InternalElt.free pool elt

    let firePastAlarms
        (t : PriorityQueue<ExternalEltValue<'a>>)
        (handleFired : ExternalElt -> unit)
        (key : Key)
        (now : TimeNs)
        : unit
        =
        let level = t.Levels.[0]

        if level.Length > 0 then
            let slot = Level.slot level key
            let slots = level.Slots
            let pool = t.Pool
            let mutable first = slots.[slot]

            if not (InternalElt.isNull first) then
                let mutable current = first
                let mutable cont = true

                while cont do
                    let elt = current
                    let next = InternalElt.next pool elt
                    if next = first then cont <- false else current <- next

                    if (InternalElt.atTime pool elt) <= now then
                        handleFired (InternalElt.toExternal elt)
                        internalRemove t elt
                        InternalElt.free pool elt
                        // We recompute [first] because [internal_remove] may have changed it.
                        first <- slots.[slot]

    let change (t : PriorityQueue<ExternalEltValue<'a>>) (elt : ExternalElt) (key : Key) (atTime : TimeNs) : unit =
        ensureValidKey t key
        let pool = t.Pool
        let elt = InternalElt.ofExternalThrowing pool elt
        internalRemove t elt
        InternalElt.setKey pool elt key
        InternalElt.setAtTime pool elt atTime
        internalAddElt t elt

    let clear (t : PriorityQueue<ExternalEltValue<'a>>) : unit =
        if not (isEmpty t) then
            t.Length <- 0
            let pool = t.Pool
            let freeElt elt = InternalElt.free pool elt
            let levels = t.Levels

            for levelIndex = 0 to Array.length levels - 1 do
                let level = levels.[levelIndex]

                if level.Length > 0 then
                    level.Length <- 0
                    let slots = level.Slots

                    for slotIndex = 0 to Array.length slots - 1 do
                        let elt = slots.[slotIndex] in

                        if not (InternalElt.isNull elt) then
                            InternalElt.iter pool elt freeElt
                            slots.[slotIndex] <- InternalElt.null'
