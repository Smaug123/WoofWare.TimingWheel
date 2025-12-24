namespace WoofWare.TimingWheel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig
open WoofWare.TimingWheel.Test.TestTimingWheelHelpers

[<TestFixture>]
module TestTimingWheelErrorCases =

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

    [<Test>]
    let ``nextAlarmFiresAtThrowing on empty wheel throws`` () =
        let t = createUnit None (Some [ 10 ]) None None

        expect {
            snapshotThrows @"System.Exception: nextAlarmFiresAtThrowing of empty timing wheel"
            return! fun () -> TimingWheel.nextAlarmFiresAtThrowing t
        }

    [<Test>]
    let ``minAlarmIntervalNumThrowing on empty wheel throws`` () =
        let t = createUnit None (Some [ 10 ]) None None

        expect {
            snapshotThrows @"System.Exception: minAlarmIntervalNumThrowing of empty timing_wheel"
            return! fun () -> TimingWheel.minAlarmIntervalNumThrowing t
        }

    [<Test>]
    let ``maxAlarmTimeInMinIntervalThrowing on empty wheel throws`` () =
        let t = createUnit None (Some [ 10 ]) None None

        expect {
            snapshotThrows @"System.Exception: maxAlarmTimeInMinIntervalThrowing of empty timing wheel"
            return! fun () -> TimingWheel.maxAlarmTimeInMinIntervalThrowing t
        }

    [<Test>]
    let ``minAlarmTimeInMinIntervalThrowing on empty wheel throws`` () =
        let t = createUnit None (Some [ 10 ]) None None

        expect {
            snapshotThrows @"System.Exception: minAlarmTimeInMinIntervalThrowing of empty timing wheel"
            return! fun () -> TimingWheel.minAlarmTimeInMinIntervalThrowing t
        }
