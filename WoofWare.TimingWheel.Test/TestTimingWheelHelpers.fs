namespace WoofWare.TimingWheel.Test

open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig

module TestTimingWheelHelpers =

    let precisions =
        [
            AlarmPrecision.oneNanosecond
            AlarmPrecision.aboutOneMicrosecond
            AlarmPrecision.aboutOneMillisecond
            AlarmPrecision.aboutOneSecond
            AlarmPrecision.aboutOneDay
        ]

    let create<'a>
        (extendToMaxNumBits : bool option)
        (levelBits : int list option)
        (start : TimeNs option)
        (alarmPrecision : TimeNs.Span option)
        : TimingWheel<ExternalEltValue<'a>>
        =
        let start = start |> Option.defaultValue TimeNs.epoch
        let alarmPrecision = alarmPrecision |> Option.defaultValue (gibiNanos 1.0)

        let config = createConfig extendToMaxNumBits levelBits alarmPrecision
        TimingWheel.create config start

    let createUnit
        (extendToMaxNumBits : bool option)
        (levelBits : int list option)
        (start : TimeNs option)
        (alarmPrecision : TimeNs.Span option)
        =
        create<unit> extendToMaxNumBits levelBits start alarmPrecision

    let printResult (r : Result<'a, 'b>) : string =
        match r with
        | Ok r -> $"(Ok {r})"
        | Error e -> $"(Error {e})"

    let advanceClockToIntervalNum
        (t : TimingWheel<ExternalEltValue<'a>>)
        (toNum : IntervalNum)
        (handleFired : ExternalElt -> unit)
        : unit
        =
        let numStart = TimingWheel.intervalNumStart t toNum
        TimingWheel.advanceClock t numStart handleFired

    let advanceClockToIntervalNumReturnRemovedIntervalNums t toTime =
        let r = ResizeArray ()
        advanceClockToIntervalNum t toTime (fun alarm -> r.Add (TimingWheel.Alarm.intervalNum t alarm))
        Seq.toList r

    /// [all_sums n] returns all combinations of positive ints that sum to [n].
    let allSums (n : int) : int list list =
        let results = Array.create (n + 1) []
        results.[0] <- [ [] ]

        for i = 1 to n do
            results.[i] <-
                List.concat (
                    List.init i (fun j -> let first = j + 1 in List.map (fun rest -> first :: rest) results.[i - first])
                )

        results.[n]

    type InitialMinAllowedIntervalNum =
        | Zero
        | Large

        static member All = [ InitialMinAllowedIntervalNum.Zero ; InitialMinAllowedIntervalNum.Large ]
