namespace WoofWare.TimingWheel

type Config =
    internal
        {
            AlarmPrecision : AlarmPrecision
            LevelBits : LevelBits
            Capacity : int option
        }

[<RequireQualifiedAccess>]
module Config =
    let alarmPrecision (t : Config) : TimeNs.Span = AlarmPrecision.toSpan t.AlarmPrecision

    let levelBitsDefault : LevelBits = LevelBits.default'

    (* [max_num_level_bits alarm_precision] returns the number of level bits needed for a
     timing wheel with the specified [alarm_precision] to be able to represent all
     possible times from [Time_ns.epoch] onward.  Since non-negative times have 62 bits,
     we require [L <= 62 - A], where [A] is the number of alarm bits and [L] is the
     number of level bits. *)
    let maxNumLevelBits (p : AlarmPrecision) : NumKeyBits =
        NumKeyBits.maxValue - (AlarmPrecision.numKeyBits p)

    let invariant (t : Config) : unit =
        if LevelBits.numBitsInternal t.LevelBits > maxNumLevelBits t.AlarmPrecision then
            failwith "expected LevelBits at most max"

        LevelBits.invariant t.LevelBits

    let create (capacity : int option) (levelBits : LevelBits) (alarmPrecision : AlarmPrecision) : Config =
        let levelBits = LevelBits.trim levelBits (maxNumLevelBits alarmPrecision)

        {
            AlarmPrecision = alarmPrecision
            LevelBits = levelBits
            Capacity = capacity
        }

    let microsecondPrecision () : Config =
        create None (LevelBits.createThrowing [ 10 ; 10 ; 6 ; 6 ; 5 ]) AlarmPrecision.aboutOneMicrosecond

    let durations (t : Config) : TimeNs.Span list =
        let init =
            AlarmPrecision.numKeyBits t.AlarmPrecision |> NumKeyBits.toInt64 |> Checked.int

        (init, t.LevelBits)
        ||> List.mapFold (fun numBitsAcc levelNumBits ->
            let numBitsAcc = numBitsAcc + (levelNumBits |> NumKeyBits.toInt64 |> Checked.int) in

            let duration =
                TimeNs.Span.ofInt64Ns (
                    if numBitsAcc = 63 then
                        System.Int64.MaxValue
                    else
                        1L <<< numBitsAcc
                )

            duration, numBitsAcc
        )
        |> fst
