namespace WoofWare.TimingWheel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig
open WoofWare.TimingWheel.Test.TestTimingWheelHelpers

[<TestFixture>]
module TestTimingWheelBasicOperations =

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

    [<Test>]
    let ``clear resets min cache so subsequent adds work correctly`` () =
        // This test verifies that after clear, the min element cache (MinElt/EltKeyLowerBound)
        // is properly reset. Without the fix, adding an element with a key higher than the
        // pre-clear minimum would leave the stale cache intact, causing nextAlarmFiresAt to
        // return incorrect results (or read freed memory).
        //
        // We add multiple elements before clear so that MinElt points to a slot that won't
        // be immediately reused when we add a single element after clear.
        let t = createUnit None (Some [ 1 ; 1 ]) None None

        // Add elements at intervals 1, 2, 3 - MinElt will point to interval 1's element
        let _ = TimingWheel.addAtIntervalNum t IntervalNum.one ()
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 2) ()
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 3) ()

        // Verify minAlarmIntervalNum returns the minimum (1)
        TimingWheel.minAlarmIntervalNum t |> shouldEqual (Some IntervalNum.one)

        // Clear the wheel - this frees all elements but without the fix, MinElt still
        // points to the freed element at interval 1, and EltKeyLowerBound is still 1
        TimingWheel.clear t

        // Add element at interval 5 first. The pool will reuse one freed slot,
        // but likely not the one MinElt points to. With the bug:
        // - EltKeyLowerBound is still 1
        // - Since 5 > 1, internalAddElt won't update MinElt
        // - MinElt still points to a freed/garbage slot
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 5) ()

        // With the fix, minAlarmIntervalNum should return 5 (the only element)
        // Without the fix, it might return garbage or the wrong value
        TimingWheel.minAlarmIntervalNum t |> shouldEqual (Some (IntervalNum.ofInt 5))

        // Now add element at interval 4 (lower than 5). This forces a min-update.
        // If the cache was properly reset after clear, the min should now be 4.
        // If the cache wasn't reset, pool reuse could have masked the bug above,
        // but this second add definitively tests that min-tracking works correctly.
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 4) ()

        TimingWheel.minAlarmIntervalNum t |> shouldEqual (Some (IntervalNum.ofInt 4))

    [<Test>]
    let ``clear resets min cache so nextAlarmFiresAt works correctly`` () =
        // This test verifies that after clear, nextAlarmFiresAt returns the correct value
        // for elements added after the clear, rather than stale data from before clear.
        let t = createUnit None (Some [ 1 ; 1 ]) None None

        // Add elements at intervals 1, 2, 3
        let _ = TimingWheel.addAtIntervalNum t IntervalNum.one ()
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 2) ()
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 3) ()

        // nextAlarmFiresAt should return the fire time for interval 1 (which is start of interval 2)
        let firesBefore = TimingWheel.nextAlarmFiresAt t

        firesBefore.IsSome |> shouldEqual true

        // Clear the wheel
        TimingWheel.clear t

        // nextAlarmFiresAt should now return None since wheel is empty
        TimingWheel.nextAlarmFiresAt t |> shouldEqual None

        // Add element at interval 5 first
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 5) ()

        // nextAlarmFiresAt should return the fire time for interval 5 (start of interval 6)
        let firesAfterFirst = TimingWheel.nextAlarmFiresAt t

        firesAfterFirst.IsSome |> shouldEqual true

        // The fire time should be for interval 6 (since alarms in interval 5 fire when we reach interval 6)
        let expectedFireTimeFor5 = TimingWheel.intervalNumStart t (IntervalNum.ofInt 6)

        firesAfterFirst |> shouldEqual (Some expectedFireTimeFor5)

        // Now add element at interval 4 (lower than 5). This forces a min-update.
        // If the cache was properly reset after clear, nextAlarmFiresAt should now
        // return the fire time for interval 4 (start of interval 5).
        // If the cache wasn't reset, pool reuse could have masked any bug above,
        // but this second add definitively tests that min-tracking works correctly.
        let _ = TimingWheel.addAtIntervalNum t (IntervalNum.ofInt 4) ()

        let firesAfterSecond = TimingWheel.nextAlarmFiresAt t

        // The fire time should be for interval 5 (since alarms in interval 4 fire when we reach interval 5)
        let expectedFireTimeFor4 = TimingWheel.intervalNumStart t (IntervalNum.ofInt 5)

        firesAfterSecond |> shouldEqual (Some expectedFireTimeFor4)

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
    let ``multiple alarms at the same time are fired in insertion order`` () =
        let t = create None None None None
        let delta = TimeNs.Span.ofSec 1.0
        let at = TimeNs.add TimeNs.epoch delta

        for i = 0 to 5 do
            TimingWheel.add t at i |> ignore

        let seen = ResizeArray ()
        TimingWheel.advanceClock t (TimeNs.add at delta) (fun v -> TimingWheel.Alarm.value t v |> seen.Add)
        Seq.toList seen |> shouldEqual [ 0..5 ]
