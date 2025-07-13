namespace WoofWare.TimingWheel

/// The timing-wheel implementation uses an array of "levels", where level [i] is an
/// array of length [2^b_i], where the [b_i] are the "level bits" specified via
/// [LevelBits.createThrowing [b_0, b_1; ...]].
///
/// A timing wheel can handle approximately [2 ** LevelBits.numBits t] intervals/keys beyond
/// the current minimum time/key, where [LevelBits.numBits t = b_0 + b_1 + ...].
///
/// One can use a [LevelBits] to trade off run time and space usage of a timing
/// wheel. For a fixed [numBits], as the number of levels increases, the length of
/// the levels decreases and the timing wheel uses less space, but the constant factor
/// for the running time of [add] and [increaseMinAllowedKey] increases.
type LevelBits = NumKeyBits list

/// The timing-wheel implementation uses an array of "levels", where level [i] is an
/// array of length [2^b_i], where the [b_i] are the "level bits" specified via
/// [LevelBits.createThrowing [b_0, b_1; ...]].
///
/// A timing wheel can handle approximately [2 ** LevelBits.numBits t] intervals/keys beyond
/// the current minimum time/key, where [LevelBits.numBits t = b_0 + b_1 + ...].
///
/// One can use a [LevelBits] to trade off run time and space usage of a timing
/// wheel. For a fixed [numBits], as the number of levels increases, the length of
/// the levels decreases and the timing wheel uses less space, but the constant factor
/// for the running time of [add] and [increaseMinAllowedKey] increases.
[<RequireQualifiedAccess>]
module LevelBits =
    let maxNumBits = int NumKeyBits.maxValue
    let numBitsInternal (t : LevelBits) = t |> List.sum

    let numBits (t : LevelBits) : int = int (numBitsInternal t)

    let internal invariant (t : LevelBits) : unit =
        if t.IsEmpty then
            failwith "LevelBits expected to be nonempty"

        for num in t do
            NumKeyBits.invariant num

            if num <= NumKeyBits.zero then
                failwith "expected strictly positive NumKeyBits"

        NumKeyBits.invariant (numBitsInternal t)

    let createThrowing' (extendToMaxNumBits : bool) (ints : int list) : LevelBits =
        if ints.IsEmpty then
            invalidArg "ints" "expected nonempty list"

        if List.exists (fun b -> b <= 0) ints then
            invalidArg "ints" "expected positive num bits"

        let numBits = List.sum ints

        if numBits > maxNumBits then
            invalidArg "ints" $"too many bits: {numBits}, more than {maxNumBits}"

        let ints =
            if extendToMaxNumBits then
                ints @ List.replicate (maxNumBits - numBits) 1
            else
                ints

        List.map NumKeyBits.ofInt ints


    let createThrowing (i : int list) : LevelBits = createThrowing' false i

    let default' : LevelBits = createThrowing [ 11 ; 10 ; 10 ; 10 ; 10 ; 10 ; 1 ]

    let internal trim (t : LevelBits) (maxNumBits : NumKeyBits) : LevelBits =
        if numBitsInternal t <= maxNumBits then
            t
        else
            let rec go t remaining =
                match t with
                | [] -> []
                | b :: t ->
                    if b >= remaining then
                        [ remaining ]
                    else
                        b :: go t (remaining - b)

            go t maxNumBits
