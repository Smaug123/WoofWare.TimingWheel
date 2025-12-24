namespace WoofWare.TimingWheel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open WoofWare.TimingWheel.Test.TestConfig
open WoofWare.TimingWheel.Test.TestTimingWheelHelpers

[<TestFixture>]
module TestTimingWheelAlarms =

    /// Check that [reschedule] and [reschedule_at_interval_num] leave an alarm in the timing wheel but reschedule its
    /// scheduled time.
    let testReschedule reschedule =
        let messages = ResizeArray ()

        let epochPlus nSeconds =
            TimeNs.add TimeNs.epoch (gibiNanos nSeconds)

        let config =
            TimingWheelConfig.create None [ 10 ] (gibiNanos 1.0 |> AlarmPrecision.ofSpanFloorPow2Ns)

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

            $"((now {TimeNs.display t.Now})\n  (next_alarm_fires_at ({Option.map TimeNs.display (TimingWheel.nextAlarmFiresAt t)}))\n  (alarm1_at ({alarmAt alarm1 |> Option.map TimeNs.display}))\n  (alarm2_at ({alarmAt alarm2 |> Option.map TimeNs.display})))"
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
    let ``test nextAlarmFiresAt`` () =
        let t = createUnit None (Some [ 10 ]) None None

        let nextAlarmFiresAfter () =
            let display =
                TimingWheel.nextAlarmFiresAt t
                |> Option.map (fun t -> TimeNs.diff t TimeNs.epoch |> TimeNs.Span.display)

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
                        TimingWheelConfig.create
                            None
                            LevelBits.default'
                            (AlarmPrecision.ofSpanFloorPow2Ns (gibiNanos 60.0))

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
            TimingWheelConfig.create None LevelBits.default' (AlarmPrecision.ofSpanFloorPow2Ns (gibiNanos 1.0))

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
            return TimingWheel.maxAlarmTimeInMinInterval t |> Option.get |> TimeNs.display
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
            return TimingWheel.maxAlarmTimeInMinInterval t |> Option.get |> TimeNs.display
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
            return TimingWheel.minAlarmTimeInMinInterval t |> Option.get |> TimeNs.display
        }

        TimingWheel.remove t a
        TimingWheel.minAlarmTimeInMinInterval t |> shouldEqual None

        let _ = addAfter (gibiNanos 2.1)
        let _ = addAfter (gibiNanos 3.9)

        expect {
            snapshot @"1970-01-01T00:00:02.2548578Z"
            return TimingWheel.minAlarmTimeInMinInterval t |> Option.get |> TimeNs.display
        }

    [<Test>]
    let ``access to a removed alarm doesn't segfault`` () =
        let config =
            TimingWheelConfig.create None LevelBits.default' (gibiNanos 1.0 |> AlarmPrecision.ofSpanFloorPow2Ns)

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
