namespace WoofWare.TimingWheel

type PriorityQueue<'a> =
    private
        {
            mutable Length : int
            mutable Pool : Pool<'a>
            mutable MinElt : InternalElt
            mutable EltKeyLowerBound : Key
            Levels : Level []
        }

type Elt = ExternalElt

module Elt =
    let null' = ExternalElt.None
    let at (p: PriorityQueue<ExternalEltValue<'a>>) (t: ExternalElt) : TimeNs=  InternalElt.atTime p.Pool (InternalElt.ofExternalThrowing p.Pool t)
    let key (p: PriorityQueue<ExternalEltValue<'a>>) (t: ExternalElt) : Key =  InternalElt.key p.Pool (InternalElt.ofExternalThrowing p.Pool t)
    let value (p: PriorityQueue<ExternalEltValue<'a>>) (t: ExternalElt) : 'a =  InternalElt.value p.Pool (InternalElt.ofExternalThrowing p.Pool t)

module PriorityQueue =
    let isEmpty (t : PriorityQueue<'a>) : bool = t.Length = 0
    let numLevels (t: PriorityQueue<'a>) : int = Array.length t.Levels
    let minAllowedKey (t: PriorityQueue<'a>) : Key = t.Levels.[0].MinAllowedKey
    let maxAllowedKey (t: PriorityQueue<'a>) : Key = t.Levels.[numLevels t - 1].MaxAllowedKey

    let internal internalIter (t : PriorityQueue<ExternalEltValue<'a>>) (f: InternalElt -> unit) : unit =
        if t.Length = 0 then () else
        let pool = t.Pool
        let levels = t.Levels
        for levelIndex = 0 to Array.length levels - 1 do
            let level = levels.[levelIndex] in
            if level.Length > 0 then
              let slots = level.Slots
              for slotIndex = 0 to Array.length slots - 1 do
                let elt = slots.[slotIndex]
                if not (InternalElt.isNull elt) then InternalElt.iter pool elt f


    let iter (t : PriorityQueue<ExternalEltValue<'a>>) (f : Elt -> unit) : unit =
        internalIter t f

    let computeDiffMaxMinAllowedKey (levelBits : NumKeyBits) (bitsPerSlot : NumKeyBits) =
        let bits = levelBits + bitsPerSlot in
        if bits = NumKeyBits.MaxValue then
            Span.maxValue
        else
            Span.pred (Key.numKeys bits)

    let invariant inv (t : PriorityQueue<ExternalEltValue<'a>>) : unit =
        let pool = t.Pool
        let levelInvariant (level : Level) : unit =
            if level.Index < 0 then failwith "expected nonnegative level"
            if level.Bits <= NumKeyBits.zero then failwith "expected strictly positive number of bits"
            if level.SlotsMask <> SlotsMask.create level.Bits then
                failwith "unexpected slots mask"
            if level.BitsPerSlot < NumKeyBits.zero then
                failwith "expected nonnegative bitsPerSlot"
            if level.KeysPerSlot <> Key.numKeys level.BitsPerSlot then
                failwith "unexpected keysPerSlot"
            if level.MinKeyInSameSlotMask <> MinKeyInSameSlotMask.create level.BitsPerSlot then
                failwith "unexpected minKeyInSameSlotMask"
            if level.DiffMaxMinAllowedKey <> computeDiffMaxMinAllowedKey level.Bits level.BitsPerSlot then
                failwith "unexpected DiffMinMaxAllowedKey"
            do
                let expectedLength =
                    (0, level.Slots)
                    ||> Array.fold (fun n elt ->
                        if InternalElt.isNull elt then n else n + InternalElt.length pool elt
                    )
                if level.Length <> expectedLength then
                    failwith "unexpected length"
            do
                if level.MinAllowedKey < Key.zero then
                    failwith "expected nonnegative MinAllowedKey"
                if level.MinAllowedKey < Key.maxValue then
                    if level.MinAllowedKey % level.KeysPerSlot <> Span.zero then
                        failwith "TODO: describe failure mode"
            if level.MaxAllowedKey <> Key.addClampToMax level.MinAllowedKey level.DiffMaxMinAllowedKey then
                failwith "unexpected MaxAllowedKey"
            do
                for elt in level.Slots do
                    if not (InternalElt.isNull elt) then
                       InternalElt.invariant pool inv elt
                       InternalElt.iter pool elt (fun elt ->
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
        if t.Length < 0 then failwith "unexpected negative length"
        // TODO: check pool invariant too
        // InternalElt.invariant t.Pool ignore
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

    // TODO: https://github.com/janestreet/core_kernel/blob/774a6821b14cbcdcde02cbbca1984ea32bf06184/timing_wheel/src/timing_wheel.ml#L1011 onward
