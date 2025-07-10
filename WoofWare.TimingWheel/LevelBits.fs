namespace WoofWare.TimingWheel

type LevelBits = NumKeyBits list

[<RequireQualifiedAccess>]
module LevelBits =
    let maxNumBits = int NumKeyBits.maxValue
    let numBitsInternal (t : LevelBits) =
        t |> List.sum

    let numBits (t : LevelBits) : int = int (numBitsInternal t)

    let invariant (t : LevelBits) : unit =
        if t.IsEmpty then failwith "LevelBits expected to be nonempty"
        for num in t do
            NumKeyBits.invariant num
            if num <= NumKeyBits.zero then failwith "expected strictly positive NumKeyBits"

        NumKeyBits.invariant (numBitsInternal t)

    let createThrowing' (extendToMaxNumBits : bool) (ints : int list) : LevelBits =
        if ints.IsEmpty then invalidArg "ints" "expected nonempty list"

        if List.exists (fun b -> b <= 0) ints then
            invalidArg "expected positive num bits" "ints"

        let numBits = List.sum ints
        if numBits > maxNumBits then
            invalidArg "ints" $"too many bits: {numBits}, more than {maxNumBits}"

        let ints =
            if extendToMaxNumBits then
                ints @ List.replicate (maxNumBits - numBits) 1
            else
                ints

        List.map NumKeyBits.ofInt ints


    let createThrowing (i: int list) : LevelBits = createThrowing' false i

    let default': LevelBits = createThrowing [ 11; 10; 10; 10; 10; 10; 1 ]

    let trim (t: LevelBits) (maxNumBits: NumKeyBits) : LevelBits =
        if numBitsInternal t <= maxNumBits then
            t
        else
          let rec go t remaining =
            match t with
            | [] -> []
            | b :: t ->
              if b >= remaining
              then [ remaining ]
              else b :: go t (remaining - b)
          go t maxNumBits

