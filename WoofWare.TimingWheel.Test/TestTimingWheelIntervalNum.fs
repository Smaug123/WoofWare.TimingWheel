namespace WoofWare.TimingWheel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig
open WoofWare.TimingWheel.Test.TestTimingWheelHelpers

[<TestFixture>]
module TestTimingWheelIntervalNum =

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

            $"intervalNum: %i{intervalNum}\nintervalNumStart: %s{TimeNs.display intervalNumStart}\nintervalStart:    %s{TimeNs.display intervalStart}"

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
    let ``intervalNum with time before epoch throws`` () =
        let t = createUnit None (Some [ 10 ]) None None
        let beforeEpoch = TimeNs.sub TimeNs.epoch (TimeNs.Span.ofInt64Ns 1L)

        expect {
            snapshotThrows
                @"System.ArgumentException: intervalNum got time too far in the past: -1, min is 0 (Parameter 'time')"

            return! fun () -> TimingWheel.intervalNum t beforeEpoch
        }
