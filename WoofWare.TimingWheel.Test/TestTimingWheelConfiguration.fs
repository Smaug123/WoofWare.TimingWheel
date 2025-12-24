namespace WoofWare.TimingWheel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig
open WoofWare.TimingWheel.Test.TestTimingWheelHelpers

[<TestFixture>]
module TestTimingWheelConfiguration =

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
  ""2189-03-16T23:50:27.6410824Z"",
  ""2262-04-11T23:47:16.8547760Z"",
  ""2262-04-11T23:47:16.8547760Z"",
  ""2262-04-11T23:47:16.8547760Z"",
  ""2262-04-11T23:47:16.8547760Z""
]"

            return
                precisions
                |> List.map (fun alarmPrecision ->
                    let config = TimingWheelConfig.create None LevelBits.default' alarmPrecision
                    let wheel = TimingWheel.create<int> config TimeNs.epoch
                    TimeNs.display (TimingWheel.maxAllowedAlarmTime wheel)
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
                    let config =
                        TimingWheelConfig.create None (LevelBits.createThrowing [ 1 ]) alarmPrecision

                    let wheel = TimingWheel.create<int> config TimeNs.epoch
                    TimeNs.display (TimingWheel.maxAllowedAlarmTime wheel)
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
                    let config =
                        TimingWheelConfig.create None (LevelBits.createThrowing [ 10 ]) alarmPrecision

                    let wheel = TimingWheel.create<int> config TimeNs.epoch
                    TimeNs.display (TimingWheel.maxAllowedAlarmTime wheel)
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
                $"{AlarmPrecision.display config.AlarmPrecision}, {LevelBits.numBits config.LevelBits}, {TimeNs.display maxAllowedAlarmTime}"

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
    let ``create with start before epoch throws`` () =
        let beforeEpoch = TimeNs.sub TimeNs.epoch (TimeNs.Span.ofInt64Ns 1L)
        let config = createConfig None (Some [ 10 ]) (gibiNanos 1.0)

        expect {
            snapshotThrows
                @"System.ArgumentException: TimingWheel.create got start -1 before the epoch (Parameter 'start')"

            return! fun () -> TimingWheel.create<unit> config beforeEpoch
        }
