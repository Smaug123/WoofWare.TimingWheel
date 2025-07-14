namespace WoofWare.TimingWheel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig

[<TestFixture>]
module TestTimingWheel =

    [<OneTimeSetUp>]
    let oneTimeSetUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let oneTimeTearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    let precisions =
        [
            AlarmPrecision.oneNanosecond
            AlarmPrecision.aboutOneMicrosecond
            AlarmPrecision.aboutOneMillisecond
            AlarmPrecision.aboutOneSecond
            AlarmPrecision.aboutOneDay
        ]

    [<Test>]
    let ``LevelBits, Config, and MaxAllowedAlarmTime`` () =
        // These times are different from those in the OCaml because we have int64 and not int63.
        (*
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1ns) (level_bits (11 10 10 10 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1.024us) (level_bits (11 10 10 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1.048576ms) (level_bits (11 10 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 19h32m48.744177664s) (level_bits (11 5))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
        *)
        expect {
            snapshotJson
                @"[
  ""2189-03-16T23:50:27.6410816Z"",
  ""2262-04-11T23:47:16.8547760Z"",
  ""2262-04-11T23:47:16.8547760Z"",
  ""2262-04-11T23:47:16.8547760Z"",
  ""2262-04-11T23:47:16.8547760Z""
]"

            return
                precisions
                |> List.map (fun alarmPrecision ->
                    let config = Config.create None LevelBits.default' alarmPrecision
                    let wheel = TimingWheel.create<int> config TimeNs.epoch
                    TimeNs.format (TimingWheel.maxAllowedAlarmTime wheel)
                )
        }

        expect {
            snapshotJson
                @"[
  ""1970-01-01T00:00:00.0000000Z"",
  ""1970-01-01T00:00:00.0000020Z"",
  ""1970-01-01T00:00:00.0020971Z"",
  ""1970-01-01T00:00:02.1474836Z"",
  ""1970-01-02T15:05:37.4883553Z""
]"

            return
                precisions
                |> List.map (fun alarmPrecision ->
                    let config = Config.create None (LevelBits.createThrowing [ 1 ]) alarmPrecision
                    let wheel = TimingWheel.create<int> config TimeNs.epoch
                    TimeNs.format (TimingWheel.maxAllowedAlarmTime wheel)
                )
        }

        expect {
            snapshotJson
                @"[
  ""1970-01-01T00:00:00.0000010Z"",
  ""1970-01-01T00:00:00.0010485Z"",
  ""1970-01-01T00:00:01.0737418Z"",
  ""1970-01-01T00:18:19.5116277Z"",
  ""1972-04-13T23:59:54.0379279Z""
]"

            return
                precisions
                |> List.map (fun alarmPrecision ->
                    let config = Config.create None (LevelBits.createThrowing [ 10 ]) alarmPrecision
                    let wheel = TimingWheel.create<int> config TimeNs.epoch
                    TimeNs.format (TimingWheel.maxAllowedAlarmTime wheel)
                )
        }

    [<Test>]
    let ``LevelBits and MaxAllowedAlarmTime with ExtendToMaxNumBits true`` () =
        let messages = ResizeArray ()

        for s in [ 1E-9 ; 1E-6 ; 1E-3 ; 1.0 ; 10.0 ] do
            let alarmPrecision = gibiNanos s
            let config = createConfig (Some true) (Some [ 1 ]) alarmPrecision
            let wheel = TimingWheel.create<int> config TimeNs.epoch
            let maxAllowedAlarmTime = TimingWheel.maxAllowedAlarmTime wheel

            messages.Add
                $"{AlarmPrecision.display config.AlarmPrecision}, {LevelBits.numBits config.LevelBits}, {TimeNs.format maxAllowedAlarmTime}"

        expect {
            snapshotJson
                @"[
  ""00s.000000000 (1 ns), 63, 2262-04-11T23:47:16.8547760Z"",
  ""00s.000001000 (1024 ns), 53, 2262-04-11T23:47:16.8547760Z"",
  ""00s.001048500 (1048576 ns), 43, 2262-04-11T23:47:16.8547760Z"",
  ""01s.073741800 (1073741824 ns), 33, 2262-04-11T23:47:16.8547760Z"",
  ""08s.589934500 (8589934592 ns), 30, 2262-04-11T23:47:16.8547760Z""
]"

            return messages
        }

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

    [<Test>]
    let ``min and maxAllowedAlarmIntervalNum`` () =
        let mutable message = ""

        let test levelBits =
            message <- ""
            let t = createUnit None (Some levelBits) None None
            TimingWheel.minAllowedAlarmIntervalNum t |> shouldEqual IntervalNum.zero

            message <-
                $"{levelBits}: {TimingWheel.minAllowedAlarmIntervalNum t}, {TimingWheel.maxAllowedAlarmIntervalNum t}"

        test [ 1 ]

        expect {
            snapshot @"[1]: 0, 1"
            return message
        }

        test [ 1 ; 1 ]

        expect {
            snapshot @"[1; 1]: 0, 5"
            return message
        }

        test [ 1 ; 1 ; 1 ]

        expect {
            snapshot @"[1; 1; 1]: 0, 11"
            return message
        }

        test [ 2 ]

        expect {
            snapshot @"[2]: 0, 3"
            return message
        }

        test [ 3 ]

        expect {
            snapshot @"[3]: 0, 7"
            return message
        }

        test [ 3 ; 1 ]

        expect {
            snapshot @"[3; 1]: 0, 23"
            return message
        }

    let printResult (r : Result<'a, 'b>) : string =
        match r with
        | Ok r -> $"(Ok {r})"
        | Error e -> $"(Error {e})"

    [<Test>]
    let ``isEmpty, intervalNum, length, mem`` () =
        let t = createUnit None (Some [ 1 ]) None None
        TimingWheel.isEmpty t |> shouldEqual true
        TimingWheel.length t |> shouldEqual 0

        let a1 = TimingWheel.addAtIntervalNum t IntervalNum.zero ()
        let a2 = TimingWheel.addAtIntervalNum t IntervalNum.zero ()

        let show () =
            let intervalNum1 =
                try
                    TimingWheel.Alarm.intervalNum t a1 |> Ok
                with e ->
                    Error e.Message

            let intervalNum2 =
                try
                    TimingWheel.Alarm.intervalNum t a2 |> Ok
                with e ->
                    Error e.Message

            $"length: {TimingWheel.length t}\nisEmpty: {TimingWheel.isEmpty t}\nintervalNum1: {printResult intervalNum1}\nintervalNum2: {printResult intervalNum2}\nmem1: {TimingWheel.mem t a1}\nmem2: {TimingWheel.mem t a2}"

        expect {
            snapshot
                @"length: 2
isEmpty: False
intervalNum1: (Ok 0)
intervalNum2: (Ok 0)
mem1: True
mem2: True"

            return show ()
        }

        TimingWheel.remove t a1

        expect {
            snapshot
                @"length: 1
isEmpty: False
intervalNum1: (Error TimingWheel got invalid alarm)
intervalNum2: (Ok 0)
mem1: False
mem2: True"

            return show ()
        }

        TimingWheel.rescheduleAtIntervalNum t a2 (IntervalNum.ofInt 1)

        expect {
            snapshot
                @"length: 1
isEmpty: False
intervalNum1: (Error TimingWheel got invalid alarm)
intervalNum2: (Ok 1)
mem1: False
mem2: True"

            return show ()
        }

        expect {
            snapshotThrows @"System.Exception: Timing_wheel cannot reschedule alarm not in timing wheel"
            return! fun () -> TimingWheel.rescheduleAtIntervalNum t a1 (IntervalNum.ofInt 1)
        }

        TimingWheel.remove t a2

        expect {
            snapshot
                @"length: 0
isEmpty: True
intervalNum1: (Error TimingWheel got invalid alarm)
intervalNum2: (Error TimingWheel got invalid alarm)
mem1: False
mem2: False"

            return show ()
        }

    let advanceClockToIntervalNum
        (t : TimingWheel<ExternalEltValue<'a>>)
        (toNum : IntervalNum)
        (handleFired : ExternalElt -> unit)
        : unit
        =
        let numStart = TimingWheel.intervalNumStart t toNum
        TimingWheel.advanceClock t numStart handleFired

    [<Test>]
    let ``add failures`` () =
        let t = createUnit None (Some [ 1 ]) None None

        let add at =
            ignore<ExternalElt> (TimingWheel.addAtIntervalNum t at ())

        let minTime t =
            IntervalNum.toIntThrowing (TimingWheel.minAllowedAlarmIntervalNum t)

        let maxTime t =
            IntervalNum.toIntThrowing (TimingWheel.maxAllowedAlarmIntervalNum t)

        for intervalNum = minTime t to maxTime t do
            add (IntervalNum.ofInt intervalNum)

        let checkAddsFail () =
            let messages = ResizeArray ()
            messages.Clear ()

            [
                IntervalNum.minValue
                IntervalNum.pred (TimingWheel.minAllowedAlarmIntervalNum t)
                IntervalNum.succ (TimingWheel.maxAllowedAlarmIntervalNum t)
                IntervalNum.maxValue
            ]
            |> List.iter (fun at ->
                let exn = Assert.Throws<exn> (fun () -> add at)
                messages.Add exn.Message
            )

            messages |> String.concat "\n"

        expect {
            snapshot
                @"intervalNumStart got too small intervalNum: -9223372036854775808, min was 0
intervalNumStart got too small intervalNum: -1, min was 0
addAtIntervalNum got invalid interval num 2 (0 .. 1)
intervalNumStart got too large intervalNum: 9223372036854775807, max was 8589934591"

            return checkAddsFail ()
        }

        advanceClockToIntervalNum t IntervalNum.one ignore

        expect {
            snapshot
                @"intervalNumStart got too small intervalNum: -9223372036854775808, min was 0
addAtIntervalNum got invalid interval num 0 (1 .. 2)
addAtIntervalNum got invalid interval num 3 (1 .. 2)
intervalNumStart got too large intervalNum: 9223372036854775807, max was 8589934591"

            return checkAddsFail ()
        }

        advanceClockToIntervalNum t (TimingWheel.maxAllowedAlarmIntervalNum t) ignore

        expect {
            snapshot
                @"intervalNumStart got too small intervalNum: -9223372036854775808, min was 0
addAtIntervalNum got invalid interval num 1 (2 .. 3)
addAtIntervalNum got invalid interval num 4 (2 .. 3)
intervalNumStart got too large intervalNum: 9223372036854775807, max was 8589934591"

            return checkAddsFail ()
        }

        TimingWheel.advanceClock t TimeNs.maxValueRepresentable ignore

        expect {
            snapshot
                @"intervalNumStart got too small intervalNum: -9223372036854775808, min was 0
addAtIntervalNum got invalid interval num 8589934590 (8589934591 .. 8589934592)
intervalNumStart got too large intervalNum: 8589934592, max was 8589934591
intervalNumStart got too large intervalNum: 9223372036854775807, max was 8589934591"

            return checkAddsFail ()
        }

    [<Test>]
    let ``test clear`` () =
        let t = createUnit None (Some [ 1 ; 1 ]) None None
        TimingWheel.clear t
        let _ = TimingWheel.addAtIntervalNum t IntervalNum.zero ()
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 2) ()

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (1, 1)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:00.0000000Z
alarms:
1970-01-01T00:00:00.0000000Z
1970-01-01T00:00:02.1474836Z
"

            return TimingWheel.display t
        }

        TimingWheel.clear t

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (1, 1)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:00.0000000Z
alarms:
"

            return TimingWheel.display t
        }

    let advanceClockToIntervalNumReturnRemovedIntervalNums t toTime =
        let r = ResizeArray ()
        advanceClockToIntervalNum t toTime (fun alarm -> r.Add (TimingWheel.Alarm.intervalNum t alarm))
        Seq.toList r

    [<Test>]
    let ``advanceClock to max interval num`` () =
        let t = createUnit None (Some [ 1 ]) None None

        let add atTime =
            ignore<ExternalElt> (TimingWheel.addAtIntervalNum t atTime ())

        add IntervalNum.zero
        add IntervalNum.one

        expect {
            snapshotThrows
                @"System.Exception: intervalNumStart got too large intervalNum: 9223372036854775807, max was 8589934591"

            return! fun () -> advanceClockToIntervalNum t IntervalNum.maxValue ignore
        }

        let maxIntervalNum = TimingWheel.intervalNum t TimeNs.maxValueRepresentable
        advanceClockToIntervalNum t maxIntervalNum ignore

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (1)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 2262-04-11T23:47:15.7810336Z
alarms:
"

            return TimingWheel.display t
        }

        add maxIntervalNum

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (1)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 2262-04-11T23:47:15.7810336Z
alarms:
2262-04-11T23:47:15.7810336Z
"

            return TimingWheel.display t
        }

    /// [all_sums n] returns all combinations of nonnegative ints that sum to [n].
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

    [<Test>]
    let ``test advanceClockToIntervalNum`` () : unit =
        let mutable numTests = 0

        let test numBits levelBits initialMinAllowedIntervalNum step =
            numTests <- numTests + 1
            let t = createUnit None (Some levelBits) None None
            let maxIntervalNum = TimingWheel.intervalNum t TimingWheel.maxTime

            let initialMinAllowedIntervalNum =
                match initialMinAllowedIntervalNum with
                | InitialMinAllowedIntervalNum.Zero -> IntervalNum.zero
                | InitialMinAllowedIntervalNum.Large ->
                    IntervalNum.sub maxIntervalNum (IntervalNumSpan.ofInt64 (1L <<< numBits))

            advanceClockToIntervalNum t initialMinAllowedIntervalNum ignore

            TimingWheel.minAllowedAlarmIntervalNum t
            |> shouldEqual initialMinAllowedIntervalNum

            TimingWheel.maxAllowedAlarmIntervalNum t
            >= (IntervalNum.add
                    (TimingWheel.minAllowedAlarmIntervalNum t)
                    (IntervalNumSpan.ofInt64 ((1L <<< numBits) - 1L)))
            |> shouldEqual true

            let intervalNums =
                List.init
                    (IntervalNum.diff
                        (TimingWheel.maxAllowedAlarmIntervalNum t)
                        (TimingWheel.minAllowedAlarmIntervalNum t)
                     |> IntervalNumSpan.toIntThrowing)
                    (fun i -> IntervalNum.add (TimingWheel.minAllowedAlarmIntervalNum t) (IntervalNumSpan.ofInt i))

            let mutable n = 0

            for at in intervalNums do
                TimingWheel.addAtIntervalNum t at () |> ignore<ExternalElt>
                n <- n + 1
                TimingWheel.length t |> shouldEqual n

            let mutable removed = []

            while TimingWheel.length t > 0 do
                let intervalNumsRemoved =
                    advanceClockToIntervalNumReturnRemovedIntervalNums
                        t
                        (IntervalNum.min
                            maxIntervalNum
                            (IntervalNum.add (TimingWheel.minAllowedAlarmIntervalNum t) step))

                removed <- intervalNumsRemoved @ removed

                for intervalNum in intervalNumsRemoved do
                    intervalNum |> shouldBeSmallerThan (TimingWheel.minAllowedAlarmIntervalNum t)

            let intervalNumsRemoved = List.sort removed
            intervalNumsRemoved |> shouldEqual intervalNums

        let numBits = 6
        let allSums = allSums numBits

        for initial in InitialMinAllowedIntervalNum.All do
            for step = 1 to 1 <<< numBits do
                for levelBits in allSums do
                    test numBits levelBits initial (IntervalNumSpan.ofInt step)

        numTests |> shouldEqual 4096

    [<Test>]
    let ``test advanceClock`` () =
        let t = createUnit None (Some [ 1 ; 1 ; 1 ; 1 ]) None None

        TimingWheel.minAlarmIntervalNum t |> shouldEqual None

        let _elt = TimingWheel.addAtIntervalNum t IntervalNum.zero ()

        TimingWheel.minAlarmIntervalNum t |> shouldEqual (Some IntervalNum.zero)

        let maxIntervalNum = 10

        for intervalNum = 1 to maxIntervalNum do
            let at = IntervalNum.ofInt intervalNum
            ignore<ExternalElt> (TimingWheel.addAtIntervalNum t at ())
            TimingWheel.minAlarmIntervalNum t |> shouldEqual (Some IntervalNum.zero)

        for intervalNum = 1 to maxIntervalNum + 1 do
            let intervalNum = IntervalNum.ofInt intervalNum

            match advanceClockToIntervalNumReturnRemovedIntervalNums t intervalNum with
            | [ intervalNum' ] -> intervalNum' |> shouldEqual (IntervalNum.pred intervalNum)
            | _ -> failwith "expected exactly one removed"

            let expected =
                if intervalNum <= IntervalNum.ofInt maxIntervalNum then
                    Some intervalNum
                else
                    None

            TimingWheel.minAlarmIntervalNum t |> shouldEqual expected

    [<Test>]
    let ``test minAlarmIntervalNum`` () =
        let t = createUnit None (Some [ 1 ; 1 ; 1 ; 1 ]) None None
        let maxIntervalNum = IntervalNum.ofInt 10

        let elts =
            List.init
                (IntervalNum.toIntThrowing maxIntervalNum + 1)
                (fun iv -> TimingWheel.addAtIntervalNum t (IntervalNum.ofInt iv) ())

        for elt in elts do
            let intervalNum = TimingWheel.Alarm.intervalNum t elt
            TimingWheel.remove t elt

            let expected =
                if intervalNum < maxIntervalNum then
                    Some (IntervalNum.succ intervalNum)
                else
                    None

            TimingWheel.minAlarmIntervalNum t |> shouldEqual expected

    [<Test>]
    let ``test iter 1`` () =
        let t = createUnit None (Some [ 1 ; 1 ; 1 ; 1 ]) None None

        let count () =
            let mutable r = 0
            TimingWheel.iter t (fun _ -> r <- r + 1)
            r

        count () |> shouldEqual 0

        let numElts = 10

        for intervalNum = 0 to numElts - 1 do
            ignore<ExternalElt> (TimingWheel.addAtIntervalNum t (IntervalNum.ofInt intervalNum) ())

        count () |> shouldEqual numElts

        advanceClockToIntervalNum t IntervalNum.one ignore
        count () |> shouldEqual (numElts - 1)

        advanceClockToIntervalNum t (IntervalNum.ofInt numElts) ignore
        count () |> shouldEqual 0

    [<Test>]
    let ``test iter 2`` () =
        let t = createUnit None (Some [ 1 ; 1 ; 1 ; 1 ]) None None
        let expected = ResizeArray ()

        for intervalNum = 0 to IntervalNum.toIntThrowing (TimingWheel.maxAllowedAlarmIntervalNum t) do
            expected.Add (TimingWheel.addAtIntervalNum t (IntervalNum.ofInt intervalNum) ())

        let expected = Seq.toList expected

        let actual = ResizeArray ()
        TimingWheel.iter t (fun elt -> actual.Add elt)
        let actual = Seq.toList actual

        let sort (elts : ExternalElt list) =
            elts |> List.sortBy (fun e1 -> TimingWheel.Alarm.intervalNum t e1)

        (sort actual) |> shouldEqual (sort expected)

    [<Test>]
    let ``start after epoch`` () =
        let t = createUnit None None (Some (TimeNs.add TimeNs.epoch (gibiNanos 1.0))) None
        TimingWheel.invariant ignore t

    [<Test>]
    let ``invalid alarm precision`` () =
        let test alarmPrecision =
            expect {
                snapshotThrows @"System.ArgumentException: expected positive span (Parameter 'span')"
                return! fun () -> createUnit None None None (Some alarmPrecision)
            }

        test (gibiNanos -1.0)
        test (gibiNanos 0.0)

    [<Test>]
    let ``test intervalNumInternal`` () =
        let messages = ResizeArray ()

        for time = -5 to 4 do
            let actual =
                AlarmPrecision.ofSpanFloorPow2Ns (TimeNs.Span.ofInt64Ns 4L)
                |> TimingWheel.intervalNumInternal (TimeNs.ofInt64NsSinceEpoch (int64<int> time))
                |> IntervalNum.toIntThrowing

            $"%i{time}: %i{actual}" |> messages.Add

        expect {
            snapshotJson
                @"[
  ""-5: -2"",
  ""-4: -1"",
  ""-3: -1"",
  ""-2: -1"",
  ""-1: -1"",
  ""0: 0"",
  ""1: 0"",
  ""2: 0"",
  ""3: 0"",
  ""4: 1""
]"

            return messages
        }

    [<Test>]
    let ``intervalNumStart, intervalStart`` () =
        let t = createUnit None None None None
        TimingWheel.mem t TimingWheel.Alarm.null' |> shouldEqual false

        let start = TimingWheel.start t

        let test after =
            let time = TimeNs.add start (gibiNanos after)
            let intervalNum = TimingWheel.intervalNum t time
            let intervalNumStart = TimingWheel.intervalNumStart t intervalNum
            let intervalStart = TimingWheel.intervalStart t time

            $"intervalNum: %i{intervalNum}\nintervalNumStart: %s{TimeNs.format intervalNumStart}\nintervalStart:    %s{TimeNs.format intervalStart}"

        expect {
            snapshot
                @"intervalNum: 0
intervalNumStart: 1970-01-01T00:00:00.0000000Z
intervalStart:    1970-01-01T00:00:00.0000000Z"

            return test 0.0
        }

        expect {
            snapshot
                @"intervalNum: 0
intervalNumStart: 1970-01-01T00:00:00.0000000Z
intervalStart:    1970-01-01T00:00:00.0000000Z"

            return test 0.1
        }

        expect {
            snapshot
                @"intervalNum: 0
intervalNumStart: 1970-01-01T00:00:00.0000000Z
intervalStart:    1970-01-01T00:00:00.0000000Z"

            return test 0.99
        }

        expect {
            snapshot
                @"intervalNum: 1
intervalNumStart: 1970-01-01T00:00:01.0737418Z
intervalStart:    1970-01-01T00:00:01.0737418Z"

            return test 1.0
        }

        expect {
            snapshot
                @"intervalNum: 1
intervalNumStart: 1970-01-01T00:00:01.0737418Z
intervalStart:    1970-01-01T00:00:01.0737418Z"

            return test 1.5
        }

        expect {
            snapshot
                @"intervalNum: 1
intervalNumStart: 1970-01-01T00:00:01.0737418Z
intervalStart:    1970-01-01T00:00:01.0737418Z"

            return test 1.99
        }

        expect {
            snapshot
                @"intervalNum: 2
intervalNumStart: 1970-01-01T00:00:02.1474836Z
intervalStart:    1970-01-01T00:00:02.1474836Z"

            return test 2.0
        }

    [<Test>]
    let ``test advanceClock 2`` () =
        let t = createUnit None None None None

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (11, 10, 10, 2)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:00.0000000Z
alarms:
"

            return TimingWheel.display t
        }

        let toTime = TimeNs.add (TimingWheel.now t) (gibiNanos 1.0)
        TimingWheel.advanceClock t toTime ignore

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (11, 10, 10, 2)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:01.0737418Z
alarms:
"

            return TimingWheel.display t
        }

    [<TestCase(false, 1)>]
    [<TestCase(false, 2)>]
    [<TestCase(true, 1)>]
    [<TestCase(true, 2)>]
    let ``min alarm at maxTime`` (advanceToMax : bool, ns : int) =
        let alarmPrecision = TimeNs.Span.scaleInt TimeNs.Span.nanosecond ns
        let t = createUnit (Some true) (Some [ 1 ]) None (Some alarmPrecision)

        if advanceToMax then
            TimingWheel.advanceClock t TimingWheel.maxTime ignore

        TimingWheel.add t TimingWheel.maxTime () |> ignore<ExternalElt>

        TimingWheel.minAlarmIntervalNumThrowing t
        |> shouldEqual (TimingWheel.intervalNum t TimingWheel.maxTime)

    let testAdvanceClockToMaxTime (ns : int) : string =
        let messages = ResizeArray ()
        let alarmPrecision = TimeNs.Span.scaleInt TimeNs.Span.nanosecond ns

        for level0Bits = 1 to 3 do
            let t =
                create<unit -> unit> (Some true) (Some [ level0Bits ]) None (Some alarmPrecision)

            messages.Add $"((alarmPrecision {alarmPrecision}ns) (level0_bits {level0Bits}))"

            for i = 10 downto 0 do
                TimingWheel.add
                    t
                    (TimeNs.sub TimingWheel.maxTime (TimeNs.Span.ofInt64Ns (int64<int> i)))
                    (fun () -> messages.Add $"(alarm (i %i{i}))")
                |> ignore<ExternalElt>

            for i = 10 downto 0 do
                messages.Add $"(advance (i {i}))"

                TimingWheel.advanceClock
                    t
                    (TimeNs.sub TimingWheel.maxTime (TimeNs.Span.ofInt64Ns (int64<int> i)))
                    (fun a -> TimingWheel.Alarm.value t a ())

        messages |> String.concat "\n"

    [<Test>]
    let ``advanceClock to max time, 1`` () =
        let messages = testAdvanceClockToMaxTime 1

        expect {
            snapshot
                @"((alarmPrecision 1ns) (level0_bits 1))
(advance (i 10))
(advance (i 9))
(alarm (i 10))
(advance (i 8))
(alarm (i 9))
(advance (i 7))
(alarm (i 8))
(advance (i 6))
(alarm (i 7))
(advance (i 5))
(alarm (i 6))
(advance (i 4))
(alarm (i 5))
(advance (i 3))
(alarm (i 4))
(advance (i 2))
(alarm (i 3))
(advance (i 1))
(alarm (i 2))
(advance (i 0))
(alarm (i 1))
((alarmPrecision 1ns) (level0_bits 2))
(advance (i 10))
(advance (i 9))
(alarm (i 10))
(advance (i 8))
(alarm (i 9))
(advance (i 7))
(alarm (i 8))
(advance (i 6))
(alarm (i 7))
(advance (i 5))
(alarm (i 6))
(advance (i 4))
(alarm (i 5))
(advance (i 3))
(alarm (i 4))
(advance (i 2))
(alarm (i 3))
(advance (i 1))
(alarm (i 2))
(advance (i 0))
(alarm (i 1))
((alarmPrecision 1ns) (level0_bits 3))
(advance (i 10))
(advance (i 9))
(alarm (i 10))
(advance (i 8))
(alarm (i 9))
(advance (i 7))
(alarm (i 8))
(advance (i 6))
(alarm (i 7))
(advance (i 5))
(alarm (i 6))
(advance (i 4))
(alarm (i 5))
(advance (i 3))
(alarm (i 4))
(advance (i 2))
(alarm (i 3))
(advance (i 1))
(alarm (i 2))
(advance (i 0))
(alarm (i 1))"

            return messages
        }

    [<Test>]
    let ``advanceClock to max time, 2`` () =
        let messages = testAdvanceClockToMaxTime 2

        expect {
            snapshot
                @"((alarmPrecision 2ns) (level0_bits 1))
(advance (i 10))
(advance (i 9))
(alarm (i 10))
(advance (i 8))
(advance (i 7))
(alarm (i 9))
(alarm (i 8))
(advance (i 6))
(advance (i 5))
(alarm (i 7))
(alarm (i 6))
(advance (i 4))
(advance (i 3))
(alarm (i 5))
(alarm (i 4))
(advance (i 2))
(advance (i 1))
(alarm (i 3))
(alarm (i 2))
(advance (i 0))
((alarmPrecision 2ns) (level0_bits 2))
(advance (i 10))
(advance (i 9))
(alarm (i 10))
(advance (i 8))
(advance (i 7))
(alarm (i 9))
(alarm (i 8))
(advance (i 6))
(advance (i 5))
(alarm (i 7))
(alarm (i 6))
(advance (i 4))
(advance (i 3))
(alarm (i 5))
(alarm (i 4))
(advance (i 2))
(advance (i 1))
(alarm (i 3))
(alarm (i 2))
(advance (i 0))
((alarmPrecision 2ns) (level0_bits 3))
(advance (i 10))
(advance (i 9))
(alarm (i 10))
(advance (i 8))
(advance (i 7))
(alarm (i 9))
(alarm (i 8))
(advance (i 6))
(advance (i 5))
(alarm (i 7))
(alarm (i 6))
(advance (i 4))
(advance (i 3))
(alarm (i 5))
(alarm (i 4))
(advance (i 2))
(advance (i 1))
(alarm (i 3))
(alarm (i 2))
(advance (i 0))"

            return messages
        }

    [<Test>]
    let ``advanceClock to max time, 4`` () =
        let messages = testAdvanceClockToMaxTime 4

        expect {
            snapshot
                @"((alarmPrecision 4ns) (level0_bits 1))
(advance (i 10))
(advance (i 9))
(advance (i 8))
(advance (i 7))
(alarm (i 10))
(alarm (i 9))
(alarm (i 8))
(advance (i 6))
(advance (i 5))
(advance (i 4))
(advance (i 3))
(alarm (i 7))
(alarm (i 6))
(alarm (i 5))
(alarm (i 4))
(advance (i 2))
(advance (i 1))
(advance (i 0))
((alarmPrecision 4ns) (level0_bits 2))
(advance (i 10))
(advance (i 9))
(advance (i 8))
(advance (i 7))
(alarm (i 10))
(alarm (i 9))
(alarm (i 8))
(advance (i 6))
(advance (i 5))
(advance (i 4))
(advance (i 3))
(alarm (i 7))
(alarm (i 6))
(alarm (i 5))
(alarm (i 4))
(advance (i 2))
(advance (i 1))
(advance (i 0))
((alarmPrecision 4ns) (level0_bits 3))
(advance (i 10))
(advance (i 9))
(advance (i 8))
(advance (i 7))
(alarm (i 10))
(alarm (i 9))
(alarm (i 8))
(advance (i 6))
(advance (i 5))
(advance (i 4))
(advance (i 3))
(alarm (i 7))
(alarm (i 6))
(alarm (i 5))
(alarm (i 4))
(advance (i 2))
(advance (i 1))
(advance (i 0))"

            return messages
        }

    [<Test>]
    let ``isEmpty, length`` () =
        let t = createUnit None None None None
        TimingWheel.isEmpty t |> shouldEqual true
        TimingWheel.length t |> shouldEqual 0

        let alarm = TimingWheel.add t t.Now ()
        TimingWheel.isEmpty t |> shouldEqual false
        TimingWheel.length t |> shouldEqual 1

        TimingWheel.remove t alarm
        TimingWheel.isEmpty t |> shouldEqual true
        TimingWheel.length t |> shouldEqual 0

    [<Test>]
    let ``test iter 3`` () =
        let t = createUnit None None None None
        TimingWheel.iter t (fun _ -> failwith "nothing to iter")

        let alarm1 = TimingWheel.add t t.Now ()
        TimingWheel.iter t (fun alarm -> alarm |> shouldEqual alarm1)

        let alarm2 = TimingWheel.add t t.Now ()

        do
            let mutable r = 0

            TimingWheel.iter
                t
                (fun alarm ->
                    r <- r + 1
                    (alarm = alarm1 || alarm = alarm2) |> shouldEqual true
                )

            r |> shouldEqual 2

        TimingWheel.remove t alarm1
        TimingWheel.remove t alarm2
        TimingWheel.iter t (fun _ -> failwith "nothing to iter")

    [<Test>]
    let ``access to a removed alarm doesn't segfault`` () =
        let config =
            Config.create None LevelBits.default' (gibiNanos 1.0 |> AlarmPrecision.ofSpanFloorPow2Ns)

        let t = TimingWheel.create config TimeNs.epoch

        let alarm = TimingWheel.add t (TimeNs.add t.Now (gibiNanos 5.0)) (ref 1)

        TimingWheel.mem t alarm |> shouldEqual true
        TimingWheel.remove t alarm

        TimingWheel.mem t alarm |> shouldEqual false

        expect {
            snapshotThrows @"System.Exception: TimingWheel got invalid alarm"
            return! fun () -> TimingWheel.Alarm.intervalNum t alarm
        }

        expect {
            snapshotThrows @"System.Exception: TimingWheel got invalid alarm"
            return! fun () -> TimingWheel.Alarm.atTime t alarm
        }

        expect {
            snapshotThrows @"System.Exception: TimingWheel got invalid alarm"
            return! fun () -> TimingWheel.Alarm.value t alarm
        }

    /// Check that [reschedule] and [reschedule_at_interval_num] leave an alarm in the timing wheel but reschedule its
    /// scheduled time.
    let testReschedule reschedule =
        let messages = ResizeArray ()

        let epochPlus nSeconds =
            TimeNs.add TimeNs.epoch (gibiNanos nSeconds)

        let config =
            Config.create None [ 10 ] (gibiNanos 1.0 |> AlarmPrecision.ofSpanFloorPow2Ns)

        let t = TimingWheel.create config (epochPlus 0.0)

        // add alarm1 before alarm2, test initial conditions
        let alarm1 = TimingWheel.add t (epochPlus 5.0) ()
        let alarm2 = TimingWheel.add t (epochPlus 10.0) ()

        let show () =
            let alarmAt alarm =
                if TimingWheel.mem t alarm then
                    Some (TimingWheel.Alarm.atTime t alarm)
                else
                    None

            $"((now {TimeNs.format t.Now})\n  (next_alarm_fires_at ({Option.map TimeNs.format (TimingWheel.nextAlarmFiresAt t)}))\n  (alarm1_at ({alarmAt alarm1 |> Option.map TimeNs.format}))\n  (alarm2_at ({alarmAt alarm2 |> Option.map TimeNs.format})))"
            |> messages.Add

        show ()
        messages.Add "Reschedule alarm1 after alarm2; alarm2 becomes next."
        reschedule t alarm1 (epochPlus 15.0)
        show ()
        messages.Add "Advance time past alarm1's original time; nothing fires."
        TimingWheel.advanceClock t (epochPlus 7.0) (fun _ -> failwith "should not fire")
        show ()
        messages.Add "Reschedule alarm1 before alarm2 again; alarm1 becomes next."
        reschedule t alarm1 (epochPlus 8.0)
        show ()
        messages.Add "Advance time past alarm1, alarm1 fires but alarm2 does not."
        let mutable hit = 0
        TimingWheel.advanceClock t (epochPlus 9.0) (fun _ -> hit <- hit + 1)
        hit |> shouldEqual 1
        show ()
        messages.Add "Cannot reschedule the already-fired alarm1."

        try
            reschedule t alarm1 (epochPlus 20.0)
            failwith "should not hit"
        with exc ->
            messages.Add exc.Message

        show ()
        messages.Add "Cannot reschedule before current time."

        try
            reschedule t alarm2 (epochPlus 8.0)
            failwith "should not hit"
        with exc ->
            messages.Add exc.Message

        show ()
        messages.Add "Cannot reschedule arbitrarily far in the future."

        try
            reschedule t alarm2 (TimeNs.add t.MaxAllowedAlarmTime (gibiNanos 1.0))
            failwith "should not hit"
        with exc ->
            messages.Add exc.Message

        messages.Add "Fire alarm 2."
        let mutable hit = 0
        TimingWheel.advanceClock t (epochPlus 11.0) (fun _ -> hit <- hit + 1)
        hit |> shouldEqual 1
        show ()

        messages |> String.concat "\n"

    [<Test>]
    let ``test reschedule`` () =
        expect {
            snapshot
                @"((now 1970-01-01T00:00:00.0000000Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:06.4424509Z)))
  (alarm1_at (Some(1970-01-01T00:00:05.3687091Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Reschedule alarm1 after alarm2; alarm2 becomes next.
((now 1970-01-01T00:00:00.0000000Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at (Some(1970-01-01T00:00:16.1061273Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Advance time past alarm1's original time; nothing fires.
((now 1970-01-01T00:00:07.5161927Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at (Some(1970-01-01T00:00:16.1061273Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Reschedule alarm1 before alarm2 again; alarm1 becomes next.
((now 1970-01-01T00:00:07.5161927Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:09.6636764Z)))
  (alarm1_at (Some(1970-01-01T00:00:08.5899345Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Advance time past alarm1, alarm1 fires but alarm2 does not.
((now 1970-01-01T00:00:09.6636764Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at ())
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Cannot reschedule the already-fired alarm1.
Timing_wheel cannot reschedule alarm not in timing wheel
((now 1970-01-01T00:00:09.6636764Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at ())
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Cannot reschedule before current time.
TimingWheel cannot schedule alarm for 8589934592 before start of current interval (9663676416)
((now 1970-01-01T00:00:09.6636764Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at ())
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Cannot reschedule arbitrarily far in the future.
TimingWheel cannot schedule alarm that far in the future (max: 1109175304191; got: 1110249046015)
Fire alarm 2.
((now 1970-01-01T00:00:11.8111600Z)
  (next_alarm_fires_at ())
  (alarm1_at ())
  (alarm2_at ()))"

            return testReschedule (fun t alarm at -> TimingWheel.reschedule t alarm at)
        }

    [<Test>]
    let ``test rescheduleAtIntervalNum`` () =
        expect {
            snapshot
                @"((now 1970-01-01T00:00:00.0000000Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:06.4424509Z)))
  (alarm1_at (Some(1970-01-01T00:00:05.3687091Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Reschedule alarm1 after alarm2; alarm2 becomes next.
((now 1970-01-01T00:00:00.0000000Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at (Some(1970-01-01T00:00:16.1061273Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Advance time past alarm1's original time; nothing fires.
((now 1970-01-01T00:00:07.5161927Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at (Some(1970-01-01T00:00:16.1061273Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Reschedule alarm1 before alarm2 again; alarm1 becomes next.
((now 1970-01-01T00:00:07.5161927Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:09.6636764Z)))
  (alarm1_at (Some(1970-01-01T00:00:08.5899345Z)))
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Advance time past alarm1, alarm1 fires but alarm2 does not.
((now 1970-01-01T00:00:09.6636764Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at ())
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Cannot reschedule the already-fired alarm1.
Timing_wheel cannot reschedule alarm not in timing wheel
((now 1970-01-01T00:00:09.6636764Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at ())
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Cannot reschedule before current time.
TimingWheel cannot schedule alarm for 8589934592 before start of current interval (9663676416)
((now 1970-01-01T00:00:09.6636764Z)
  (next_alarm_fires_at (Some(1970-01-01T00:00:11.8111600Z)))
  (alarm1_at ())
  (alarm2_at (Some(1970-01-01T00:00:10.7374182Z))))
Cannot reschedule arbitrarily far in the future.
TimingWheel cannot schedule alarm that far in the future (max: 1109175304191; got: 1109175304192)
Fire alarm 2.
((now 1970-01-01T00:00:11.8111600Z)
  (next_alarm_fires_at ())
  (alarm1_at ())
  (alarm2_at ()))"

            return
                testReschedule (fun t alarm at ->
                    TimingWheel.rescheduleAtIntervalNum t alarm (TimingWheel.intervalNum t at)
                )
        }

    [<Test>]
    let ``advanceClock fires alarms at the right time`` () =
        let test add numAlarms alarmPrecision alarmSeparation advanceBy =
            let config =
                Config.create None LevelBits.default' (AlarmPrecision.ofSpanFloorPow2Ns alarmPrecision)

            let t = TimingWheel.create config TimeNs.epoch

            for i = 1 to numAlarms do
                let at = TimeNs.add t.Now (TimeNs.Span.scale alarmSeparation (float<int> i))
                ignore (add t at (fun () -> at <= t.Now |> shouldEqual true))

            while not (TimingWheel.isEmpty t) do
                let toTime = TimeNs.add t.Now advanceBy
                TimingWheel.advanceClock t toTime (fun alarm -> TimingWheel.Alarm.value t alarm ())
                TimingWheel.nowIntervalNum t |> shouldEqual (TimingWheel.intervalNum t toTime)

        for add in
            [
                TimingWheel.add
                fun t at -> TimingWheel.addAtIntervalNum t (TimingWheel.intervalNum t at)
            ] do
            for numAlarms in [ 100 ] do
                for s in [ 1.0 ; 0.5 ; 0.1 ] do
                    let alarmPrecision = gibiNanos s

                    for s in [ 0.01 ; 0.1 ; 0.5 ; 1.0 ; 2.0 ; 10.0 ] do
                        let alarmSeparation = gibiNanos s

                        for s in [ 0.1 ; 0.5 ; 1.0 ; 2.0 ; 10.0 ] do
                            let advanceBy = gibiNanos s
                            test add numAlarms alarmPrecision alarmSeparation advanceBy

    [<Test>]
    let ``add and advanceClock`` () =
        let config =
            Config.create None [ 10 ] (AlarmPrecision.ofSpanFloorPow2Ns (gibiNanos 1.0))

        let t = TimingWheel.create config TimeNs.epoch

        let add after f =
            ignore (TimingWheel.add t (TimeNs.add t.Now after) f)

        let advanceClock by =
            TimingWheel.advanceClock t (TimeNs.add t.Now by) (fun alarm -> TimingWheel.Alarm.value t alarm ())

        expect {
            snapshotThrows
                @"System.Exception: TimingWheel cannot schedule alarm for -1073741824 before start of current interval (0)"

            return! fun () -> add (gibiNanos -1.0) ignore
        }

        TimeNs.add t.MaxAllowedAlarmTime TimeNs.Span.nanosecond
        |> shouldEqual (TimeNs.add t.Now (gibiNanos 1024.0))

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (10)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:00.0000000Z
alarms:
"

            return TimingWheel.display t
        }

        add (gibiNanos 30.0) ignore

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (10)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:00.0000000Z
alarms:
1970-01-01T00:00:32.2122547Z
"

            return TimingWheel.display t
        }

        advanceClock (gibiNanos 30.0)

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (10)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:32.2122547Z
alarms:
1970-01-01T00:00:32.2122547Z
"

            return TimingWheel.display t
        }

        advanceClock (gibiNanos 1.0)

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (10)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:33.2859965Z
alarms:
"

            return TimingWheel.display t
        }

    [<Test>]
    let ``test nextAlarmFiresAt`` () =
        let t = createUnit None (Some [ 10 ]) None None

        let nextAlarmFiresAfter () =
            let display =
                TimingWheel.nextAlarmFiresAt t
                |> Option.map (fun t -> TimeNs.diff t TimeNs.epoch |> Span.display)

            $"nextAlarmFiresAfter: {display}"

        let addAt at =
            TimingWheel.add t (TimeNs.add TimeNs.epoch at) () |> ignore
            nextAlarmFiresAfter ()

        let advanceClock span =
            TimingWheel.advanceClock t (TimeNs.add TimeNs.epoch span) ignore
            nextAlarmFiresAfter ()

        expect {
            snapshot @"nextAlarmFiresAfter: Some(03s.221225400)"
            return addAt (gibiNanos 2.0)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(02s.147483600)"
            return addAt (gibiNanos 1.5)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(02s.147483600)"
            return addAt (gibiNanos 1.0)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(01s.073741800)"
            return addAt (gibiNanos 0.5)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(01s.073741800)"
            return addAt (gibiNanos 0.1)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(01s.073741800)"
            return advanceClock (gibiNanos 0.5)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(02s.147483600)"
            return advanceClock (gibiNanos 1.0)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(02s.147483600)"
            return advanceClock (gibiNanos 1.5)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: Some(03s.221225400)"
            return advanceClock (gibiNanos 2.0)
        }

        expect {
            snapshot @"nextAlarmFiresAfter: "
            return advanceClock (gibiNanos 3.0)
        }

    [<Test>]
    let ``nextAlarmFiresAt with an alarm at maxTime`` () =
        let t = createUnit (Some true) (Some [ 1 ]) None None
        TimingWheel.add t TimingWheel.maxTime () |> ignore<ExternalElt>
        TimingWheel.nextAlarmFiresAt t |> shouldEqual None

        expect {
            snapshotThrows @"System.Exception: nextAlarmFiresAtThrowing with all alarms in max interval"
            return! fun () -> TimingWheel.nextAlarmFiresAtThrowing t
        }

        expect {
            snapshot
                @"config: 01s.073741800 (1073741824 ns) : (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
start: 1970-01-01T00:00:00.0000000Z
maxIntervalNum: 8589934591
now: 1970-01-01T00:00:00.0000000Z
alarms:
2262-04-11T23:47:16.8547760Z
"

            return TimingWheel.display t
        }

    [<Test>]
    let ``firePastAlarms - all possible subsets of alarms in the first bucket that fire`` () =
        let start = TimeNs.epoch
        let at sec = TimeNs.add start (gibiNanos sec)
        let at1 = at 1.0
        let at2 = at 2.0
        let mutable numTests = 0

        for numElts = 0 to 5 do
            let rec loop i ats =
                numTests <- numTests + 1

                if i > 0 then
                    loop (i - 1) (at1 :: ats)
                    loop (i - 1) (at2 :: ats)
                else
                    let config =
                        Config.create None LevelBits.default' (AlarmPrecision.ofSpanFloorPow2Ns (gibiNanos 60.0))

                    let t = TimingWheel.create config start
                    let mutable numFired = 0

                    for at in ats do
                        let alarm = TimingWheel.add t at ()
                        TimingWheel.Alarm.intervalNum t alarm |> shouldEqual IntervalNum.zero

                    TimingWheel.advanceClock t at1 (fun _ -> failwith "should not fire")

                    TimingWheel.firePastAlarms
                        t
                        (fun alarm ->
                            if at1 = TimingWheel.Alarm.atTime t alarm then
                                numFired <- numFired + 1
                            else
                                failwith "wrong"
                        )

                    numFired |> shouldEqual (ats |> List.filter (fun s -> s = at1) |> List.length)

            loop numElts []

        numTests |> shouldEqual 120

    [<Test>]
    let ``alarm buckets`` () =
        let start = TimeNs.epoch

        let config =
            Config.create None LevelBits.default' (AlarmPrecision.ofSpanFloorPow2Ns (gibiNanos 1.0))

        let t = TimingWheel.create<bool ref> config start

        let handleFired (a : TimingWheel.Alarm) : unit =
            let r = TimingWheel.Alarm.value t a
            r.Value |> shouldEqual false
            r.Value <- true

        let precision = TimingWheel.alarmPrecision t
        let precision02 = TimeNs.Span.scale precision 0.2

        TimingWheel.add t (TimeNs.add start precision) (ref false)
        |> ignore<ExternalElt>

        let base' = TimingWheel.nextAlarmFiresAt t |> Option.get
        let step0 = TimeNs.add base' precision02
        let step1 = TimeNs.add step0 precision02
        let step2 = TimeNs.add step1 precision02
        let step3 = TimeNs.add step2 precision
        // Check all alarm will be in the same bucket but step3
        let intervalNum0 = TimingWheel.intervalNum t step0
        let intervalNum1 = TimingWheel.intervalNum t step1
        let intervalNum2 = TimingWheel.intervalNum t step2
        let intervalNum3 = TimingWheel.intervalNum t step3
        intervalNum0 |> IntervalNum.toIntThrowing |> shouldEqual 2
        intervalNum1 |> IntervalNum.toIntThrowing |> shouldEqual 2
        intervalNum2 |> IntervalNum.toIntThrowing |> shouldEqual 2
        intervalNum3 |> IntervalNum.toIntThrowing |> shouldEqual 3

        let step1Fired = ref false
        let step2Fired = ref false
        let step3Fired = ref false
        TimingWheel.add t step1 step1Fired |> ignore<ExternalElt>
        TimingWheel.add t step2 step2Fired |> ignore<ExternalElt>
        TimingWheel.add t step3 step3Fired |> ignore<ExternalElt>

        let show () =
            [ step1Fired.Value ; step2Fired.Value ; step3Fired.Value ]

        // Nothing should be triggered before
        TimingWheel.advanceClock t step0 handleFired
        TimingWheel.firePastAlarms t handleFired
        show () |> shouldEqual [ false ; false ; false ]

        TimingWheel.advanceClock t step1 handleFired
        show () |> shouldEqual [ false ; false ; false ]
        TimingWheel.firePastAlarms t handleFired
        show () |> shouldEqual [ true ; false ; false ]

        TimingWheel.advanceClock t step2 handleFired
        show () |> shouldEqual [ true ; false ; false ]
        TimingWheel.firePastAlarms t handleFired
        show () |> shouldEqual [ true ; true ; false ]

    [<Test>]
    let ``test maxAlarmTimeInMinInterval`` () =
        let t = create<unit -> unit> None (Some [ 1 ; 1 ]) None None

        TimingWheel.maxAlarmTimeInMinInterval t |> shouldEqual None

        let addAfter span =
            TimingWheel.add t (TimeNs.add t.Now span) ignore

        let a = addAfter (gibiNanos 0.5)

        expect {
            snapshot @"1970-01-01T00:00:00.5368709Z"
            return TimingWheel.maxAlarmTimeInMinInterval t |> Option.get |> TimeNs.format
        }

        TimingWheel.remove t a
        TimingWheel.maxAlarmTimeInMinInterval t |> shouldEqual None

        // Add two alarms that appear in different intervals, but in the same slot on the
        //   second level of the timing wheel.  So the correct [max_alarm_time_in_min_interval] is
        //   2.1, not 3.9.
        let _ = addAfter (gibiNanos 2.1)
        let _ = addAfter (gibiNanos 3.9)

        expect {
            snapshot @"1970-01-01T00:00:02.2548578Z"
            return TimingWheel.maxAlarmTimeInMinInterval t |> Option.get |> TimeNs.format
        }

    [<Test>]
    let ``test minAlarmTimeInMinInterval`` () =
        let t = create<unit -> unit> None (Some [ 1 ; 1 ]) None None

        TimingWheel.minAlarmTimeInMinInterval t |> shouldEqual None

        let addAfter span =
            TimingWheel.add t (TimeNs.add t.Now span) ignore

        let a = addAfter (gibiNanos 0.5)

        expect {
            snapshot @"1970-01-01T00:00:00.5368709Z"
            return TimingWheel.maxAlarmTimeInMinInterval t |> Option.get |> TimeNs.format
        }

        TimingWheel.remove t a
        TimingWheel.maxAlarmTimeInMinInterval t |> shouldEqual None

        let _ = addAfter (gibiNanos 2.1)
        let _ = addAfter (gibiNanos 3.9)

        expect {
            snapshot @"1970-01-01T00:00:02.2548578Z"
            return TimingWheel.maxAlarmTimeInMinInterval t |> Option.get |> TimeNs.format
        }

    [<Test>]
    let ``multiple alarms at the same time are fired in insertion order`` () =
        let t = create None None None None
        let delta = TimeNs.Span.ofSec 1.0
        let at = TimeNs.add TimeNs.epoch delta

        for i = 0 to 5 do
            TimingWheel.add t at i |> ignore

        let seen = ResizeArray ()
        TimingWheel.advanceClock t (TimeNs.add at delta) (fun v -> TimingWheel.Alarm.value t v |> seen.Add)
        Seq.toList seen |> shouldEqual [ 0..5 ]

    [<Test>]
    let ``maxAlarmTime cannot be exceeded by add or addAtIntervalNum`` () =
        let t = create None (Some [ 10 ]) None None

        let succ t =
            let r = TimeNs.add t TimeNs.Span.nanosecond
            r |> shouldBeGreaterThan t
            r

        let badTime = succ t.MaxAllowedAlarmTime

        expect {
            snapshotThrows
                @"System.Exception: TimingWheel cannot schedule alarm that far in the future (max: 1099511627775; got: 1099511627776)"

            return! fun () -> TimingWheel.add t badTime ()
        }

        expect {
            snapshotThrows @"System.Exception: addAtIntervalNum got invalid interval num 1024 (0 .. 1023)"

            return!
                fun () ->
                    TimingWheel.addAtIntervalNum
                        t
                        (IntervalNum.succ (TimingWheel.intervalNum t t.MaxAllowedAlarmTime))
                        ()
        }
