namespace WoofWare.TimingWheel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig
open WoofWare.TimingWheel.Test.TestTimingWheelHelpers

[<TestFixture>]
module TestTimingWheelAdvanceClock =

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
    let ``advanceClock fires alarms at the right time`` () =
        let test add numAlarms alarmPrecision alarmSeparation advanceBy =
            let config =
                TimingWheelConfig.create None LevelBits.default' (AlarmPrecision.ofSpanFloorPow2Ns alarmPrecision)

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
            TimingWheelConfig.create None [ 10 ] (AlarmPrecision.ofSpanFloorPow2Ns (gibiNanos 1.0))

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
    let ``advanceClockStopAtNextAlarm stops at alarm's minAlarmTime without firing`` () =
        // advanceClockStopAtNextAlarm advances time to min(toTime, minAlarmTime of next alarm)
        // This is the actual scheduled time of the alarm, not the "fire time" (next interval start).
        // The alarm does NOT fire - it only fires when we advance past its interval.
        let t = createUnit None (Some [ 10 ]) None None

        // Add an alarm at interval 5 - this schedules it at intervalNumStart(5)
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 5) ()

        // The alarm's minAlarmTime is the start of interval 5
        let alarmTime = TimingWheel.intervalNumStart t (IntervalNum.ofInt 5)

        // Try to advance well past the alarm with advanceClockStopAtNextAlarm
        let farFuture = TimingWheel.intervalNumStart t (IntervalNum.ofInt 100)

        TimingWheel.advanceClockStopAtNextAlarm t farFuture (fun _ -> failwith "should not fire")

        // Time should have stopped at the alarm's scheduled time (minAlarmTime)
        TimingWheel.now t |> shouldEqual alarmTime

        // The alarm has NOT fired yet - we've stopped AT its time, not past its interval
        TimingWheel.length t |> shouldEqual 1

        // Now use firePastAlarms to fire alarms whose exact time has passed
        let mutable fired = 0
        TimingWheel.firePastAlarms t (fun _ -> fired <- fired + 1)
        fired |> shouldEqual 1

        // Wheel should be empty now
        TimingWheel.isEmpty t |> shouldEqual true

    [<Test>]
    let ``advanceClockStopAtNextAlarm stops before alarm if toTime is earlier`` () =
        let t = createUnit None (Some [ 10 ]) None None

        // Add an alarm at interval 10
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 10) ()

        // Try to advance to interval 5 (before the alarm)
        let earlyTime = TimingWheel.intervalNumStart t (IntervalNum.ofInt 5)

        TimingWheel.advanceClockStopAtNextAlarm t earlyTime (fun _ -> failwith "should not fire")

        // Time should be at earlyTime
        TimingWheel.now t |> shouldEqual earlyTime

        // Alarm should still be there
        TimingWheel.length t |> shouldEqual 1

    [<Test>]
    let ``advanceClockStopAtNextAlarm on empty wheel advances to toTime`` () =
        let t = createUnit None (Some [ 10 ]) None None

        let targetTime = TimingWheel.intervalNumStart t (IntervalNum.ofInt 50)

        TimingWheel.advanceClockStopAtNextAlarm t targetTime (fun _ -> failwith "should not fire")

        TimingWheel.now t |> shouldEqual targetTime

    [<Test>]
    let ``advanceClockStopAtNextAlarm interaction with firePastAlarms`` () =
        // Test that advanceClockStopAtNextAlarm + firePastAlarms can be used to
        // precisely control firing of alarms within an interval
        let t = create<int> None (Some [ 10 ]) None None

        // Add multiple alarms at the same interval but different exact times
        let baseTime = TimingWheel.intervalNumStart t (IntervalNum.ofInt 5)
        let offset1 = TimeNs.Span.ofInt64Ns 100L
        let offset2 = TimeNs.Span.ofInt64Ns 200L

        let _ = TimingWheel.add t (TimeNs.add baseTime offset1) 1
        let _ = TimingWheel.add t (TimeNs.add baseTime offset2) 2

        // Both should be in interval 5
        TimingWheel.minAlarmIntervalNum t |> shouldEqual (Some (IntervalNum.ofInt 5))

        // Advance to just past the first alarm's exact time (within interval 5)
        let afterFirst = TimeNs.add baseTime (TimeNs.Span.ofInt64Ns 150L)
        let mutable firedValues = []

        // advanceClockStopAtNextAlarm won't fire within-interval alarms until we reach the next interval
        TimingWheel.advanceClockStopAtNextAlarm
            t
            afterFirst
            (fun a -> firedValues <- TimingWheel.Alarm.value t a :: firedValues)

        // Nothing should have fired yet since we're still in interval 5
        firedValues |> shouldEqual []
        TimingWheel.length t |> shouldEqual 2

        // Now use firePastAlarms to fire alarms whose exact time has passed
        TimingWheel.firePastAlarms t (fun a -> firedValues <- TimingWheel.Alarm.value t a :: firedValues)

        // Only the first alarm (at offset1 = 100ns) should have fired
        firedValues |> shouldEqual [ 1 ]
        TimingWheel.length t |> shouldEqual 1

        // Advance past the second alarm's time and use firePastAlarms again
        let afterSecond = TimeNs.add baseTime (TimeNs.Span.ofInt64Ns 250L)
        TimingWheel.advanceClockStopAtNextAlarm t afterSecond (fun _ -> failwith "should not fire via advanceClock")
        TimingWheel.firePastAlarms t (fun a -> firedValues <- TimingWheel.Alarm.value t a :: firedValues)

        firedValues |> shouldEqual [ 2 ; 1 ]
        TimingWheel.length t |> shouldEqual 0
