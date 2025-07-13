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
                    TimeNs.format wheel.MaxAllowedAlarmTime
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
                    TimeNs.format wheel.MaxAllowedAlarmTime
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
                    TimeNs.format wheel.MaxAllowedAlarmTime
                )
        }

    [<Test>]
    let ``LevelBits and MaxAllowedAlarmTime with ExtendToMaxNumBits true`` () =
        let messages = ResizeArray ()

        for s in [ 1E-9 ; 1E-6 ; 1E-3 ; 1.0 ; 10.0 ] do
            let alarmPrecision = gibiNanos s
            let config = createConfig (Some true) (Some [ 1 ]) alarmPrecision
            let wheel = TimingWheel.create<int> config TimeNs.epoch
            let maxAllowedAlarmTime = wheel.MaxAllowedAlarmTime

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

    let createUnit
        (extendToMaxNumBits : bool option)
        (levelBits : int list option)
        (start : TimeNs option)
        (alarmPrecision : TimeNs option)
        =
        let start = start |> Option.defaultValue TimeNs.epoch
        let alarmPrecision = alarmPrecision |> Option.defaultValue (gibiNanos 1.0)

        let config = createConfig extendToMaxNumBits levelBits alarmPrecision
        TimingWheel.create<unit> config start

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
        TimingWheel.advanceClock t (TimingWheel.intervalNumStart t toNum) handleFired

    [<Test>]
    let ``add failures`` () =
        let t = createUnit None (Some [ 1 ]) None None

        let add at =
            ignore (TimingWheel.addAtIntervalNum t at)

        for intervalNum = IntervalNum.toIntThrowing (TimingWheel.minAllowedAlarmIntervalNum t) to IntervalNum
            .toIntThrowing (TimingWheel.maxAllowedAlarmIntervalNum t) do
            add (IntervalNum.ofInt intervalNum)

        let checkAddsFail () =
            [
                IntervalNum.minValue
                IntervalNum.pred (TimingWheel.minAllowedAlarmIntervalNum t)
                IntervalNum.succ (TimingWheel.maxAllowedAlarmIntervalNum t)
                IntervalNum.maxValue
            ]
            |> List.iter (fun at ->
                expect {
                    snapshotThrows ""
                    return! fun () -> add at
                }
            )

        checkAddsFail ()
        advanceClockToIntervalNum t IntervalNum.one ignore
        checkAddsFail ()
        advanceClockToIntervalNum t (TimingWheel.maxAllowedAlarmIntervalNum t) ignore
        checkAddsFail ()
        advanceClockToIntervalNum t TimeNs.maxValueRepresentable ignore
        checkAddsFail ()
(*
let%expect_test "[add] failures" =
  let t = create_unit () ~level_bits:[ 1 ] in
  let add ~at = ignore (add_at_interval_num t ~at () : _ Alarm.t) in
  for
    interval_num = Interval_num.to_int_exn (min_allowed_alarm_interval_num t)
    to Interval_num.to_int_exn (max_allowed_alarm_interval_num t)
  do
    add ~at:(Interval_num.of_int interval_num)
  done;
  let check_adds_fail () =
    List.iter
      [ Interval_num.min_value
      ; Interval_num.pred (min_allowed_alarm_interval_num t)
      ; Interval_num.succ (max_allowed_alarm_interval_num t)
      ; Interval_num.max_value
      ]
      ~f:(fun at -> require_does_raise (fun () -> add ~at))
  in
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -1)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   2)
     (min_allowed_alarm_interval_num 0)
     (max_allowed_alarm_interval_num 1))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295))
    |}];
  advance_clock_to_interval_num t ~to_:Interval_num.one ~handle_fired:ignore;
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   0)
     (min_allowed_alarm_interval_num 1)
     (max_allowed_alarm_interval_num 2))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   3)
     (min_allowed_alarm_interval_num 1)
     (max_allowed_alarm_interval_num 2))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295))
    |}];
  advance_clock_to_interval_num
    t
    ~to_:(max_allowed_alarm_interval_num t)
    ~handle_fired:ignore;
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   1)
     (min_allowed_alarm_interval_num 2)
     (max_allowed_alarm_interval_num 3))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   4)
     (min_allowed_alarm_interval_num 2)
     (max_allowed_alarm_interval_num 3))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295))
    |}];
  advance_clock t ~to_:Private.max_time ~handle_fired:ignore;
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   4_294_967_294)
     (min_allowed_alarm_interval_num 4_294_967_295)
     (max_allowed_alarm_interval_num 4_294_967_296))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_294_967_296)
     (t.max_interval_num 4_294_967_295))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295))
    |}]
;;

let%expect_test "[clear]" =
  let t = create_unit () ~level_bits:[ 1; 1 ] in
  clear t;
  let _e1 = add_at_interval_num t ~at:Interval_num.zero () in
  let _e2 = add_at_interval_num t ~at:(Interval_num.of_int 2) () in
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (1 1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms (
       ((at "1970-01-01 00:00:00Z")           (value _))
       ((at "1970-01-01 00:00:02.147483648Z") (value _)))))
    |}];
  clear t;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (1 1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms ()))
    |}]
;;

let advance_clock_to_interval_num_return_removed_interval_nums t ~to_ =
  let r = ref [] in
  let handle_fired alarm = r := Alarm.interval_num t alarm :: !r in
  advance_clock_to_interval_num t ~to_ ~handle_fired;
  !r
;;

let%expect_test "[advance_clock] to max interval num" =
  let t = create_unit () ~level_bits:[ 1 ] in
  let add ~at = ignore (add_at_interval_num t ~at () : _ Alarm.t) in
  add ~at:Interval_num.zero;
  add ~at:Interval_num.one;
  require_does_raise (fun () ->
    advance_clock_to_interval_num t ~to_:Interval_num.max_value ~handle_fired:ignore);
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295))
    |}];
  let max_interval_num = interval_num t Private.max_time in
  advance_clock_to_interval_num t ~to_:max_interval_num ~handle_fired:ignore;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "2116-02-20 23:53:37.35364608Z")
     (alarms ()))
    |}];
  add ~at:max_interval_num;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "2116-02-20 23:53:37.35364608Z")
     (alarms ((
       (at    "2116-02-20 23:53:37.35364608Z")
       (value _)))))
    |}]
;;

let%expect_test "[advance_clock_to_interval_num]" =
  let num_tests = ref 0 in
  (* [all_sums n] returns all combinations of nonnegative ints that sum to [n]. *)
  let all_sums n =
    let results = Array.create ~len:(n + 1) [] in
    results.(0) <- [ [] ];
    for i = 1 to n do
      results.(i)
      <- List.concat
           (List.init i ~f:(fun j ->
              let first = j + 1 in
              List.map results.(i - first) ~f:(fun rest -> first :: rest)))
    done;
    results.(n)
  in
  let module Initial_min_allowed_interval_num = struct
    type t =
      | Zero
      | Large
    [@@deriving enumerate, sexp_of]
  end
  in
  let test
    ~num_bits
    ~level_bits
    ~(initial_min_allowed_interval_num : Initial_min_allowed_interval_num.t)
    ~step
    =
    incr num_tests;
    let t = create_unit () ~level_bits in
    let max_interval_num = interval_num t Private.max_time in
    let initial_min_allowed_interval_num =
      match initial_min_allowed_interval_num with
      | Zero -> Interval_num.zero
      | Large ->
        Interval_num.sub
          max_interval_num
          (Interval_num.Span.of_int63 (Int63.shift_left Int63.one num_bits))
    in
    try
      advance_clock_to_interval_num
        t
        ~to_:initial_min_allowed_interval_num
        ~handle_fired:ignore;
      require_equal
        (module Interval_num)
        (min_allowed_alarm_interval_num t)
        initial_min_allowed_interval_num;
      require
        (Interval_num.( >= )
           (max_allowed_alarm_interval_num t)
           (Interval_num.add
              (min_allowed_alarm_interval_num t)
              (Interval_num.Span.of_int63
                 (Int63.( - ) (Int63.shift_left Int63.one num_bits) Int63.one))));
      let interval_nums =
        List.init
          (Interval_num.Span.to_int_exn
             (Interval_num.diff
                (max_allowed_alarm_interval_num t)
                (min_allowed_alarm_interval_num t)))
          ~f:(fun i ->
            Interval_num.add
              (min_allowed_alarm_interval_num t)
              (Interval_num.Span.of_int i))
      in
      let n = ref 0 in
      List.iter interval_nums ~f:(fun at ->
        ignore (add_at_interval_num t ~at () : _ Alarm.t);
        incr n;
        require (length t = !n));
      let removed = ref [] in
      while length t > 0 do
        let interval_nums_removed =
          advance_clock_to_interval_num_return_removed_interval_nums
            t
            ~to_:
              (Interval_num.min
                 max_interval_num
                 (Interval_num.add (min_allowed_alarm_interval_num t) step))
        in
        removed := interval_nums_removed @ !removed;
        List.iter interval_nums_removed ~f:(fun interval_num ->
          require (Interval_num.( < ) interval_num (min_allowed_alarm_interval_num t)))
      done;
      let interval_nums_removed = List.sort !removed ~compare:Interval_num.compare in
      require (Poly.equal interval_nums_removed interval_nums)
    with
    | exn -> failwiths "failure" (exn, t) [%sexp_of: exn * _ t]
  in
  let num_bits = 6 in
  let all_sums = all_sums num_bits in
  List.iter
    Initial_min_allowed_interval_num.all
    ~f:(fun initial_min_allowed_interval_num ->
      for step = 1 to 1 lsl num_bits do
        List.iter all_sums ~f:(fun level_bits ->
          test
            ~num_bits
            ~level_bits
            ~initial_min_allowed_interval_num
            ~step:(Interval_num.Span.of_int step))
      done);
  print_s [%message (num_tests : int ref)];
  [%expect {| (num_tests 4_096) |}]
;;

module Interval_num_option = struct
  type t = Interval_num.t option [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
end

let%expect_test "[advance_clock]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  require (is_none (min_alarm_interval_num t));
  let _elt = add_at_interval_num t ~at:Interval_num.zero () in
  require_equal
    (module Interval_num_option)
    (min_alarm_interval_num t)
    (Some Interval_num.zero);
  let max_interval_num = 10 in
  for interval_num = 1 to max_interval_num do
    let at = Interval_num.of_int interval_num in
    require_does_not_raise (fun () -> ignore (add_at_interval_num t ~at () : _ Alarm.t));
    require_equal
      (module Interval_num_option)
      (min_alarm_interval_num t)
      (Some Interval_num.zero)
  done;
  for interval_num = 1 to max_interval_num + 1 do
    let interval_num = Interval_num.of_int interval_num in
    (match
       advance_clock_to_interval_num_return_removed_interval_nums t ~to_:interval_num
     with
     | [ interval_num' ] ->
       require_equal (module Interval_num) interval_num' (Interval_num.pred interval_num)
     | _ -> require false);
    require_equal
      (module Interval_num_option)
      (min_alarm_interval_num t)
      (if Interval_num.( <= ) interval_num (Interval_num.of_int max_interval_num)
       then Some interval_num
       else None)
  done
;;

let%expect_test "[min_alarm_interval_num]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  let max_interval_num = Interval_num.of_int 10 in
  let elts =
    List.init
      (Interval_num.to_int_exn max_interval_num + 1)
      ~f:(fun interval_num ->
        add_at_interval_num t ~at:(Interval_num.of_int interval_num) ())
  in
  List.iter elts ~f:(fun elt ->
    let interval_num = Alarm.interval_num t elt in
    remove t elt;
    require_equal
      (module Interval_num_option)
      (min_alarm_interval_num t)
      (if Interval_num.( < ) interval_num max_interval_num
       then Some (Interval_num.succ interval_num)
       else None))
;;

let%expect_test "[iter]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  let count () =
    let r = ref 0 in
    iter t ~f:(fun _ -> incr r);
    !r
  in
  let show_count () = print_s [%sexp (count () : int)] in
  show_count ();
  [%expect {| 0 |}];
  let num_elts = 10 in
  for interval_num = 0 to num_elts - 1 do
    ignore (add_at_interval_num t ~at:(Interval_num.of_int interval_num) () : _ Alarm.t)
  done;
  show_count ();
  [%expect {| 10 |}];
  advance_clock_to_interval_num t ~to_:Interval_num.one ~handle_fired:ignore;
  show_count ();
  [%expect {| 9 |}];
  advance_clock_to_interval_num t ~to_:(Interval_num.of_int num_elts) ~handle_fired:ignore;
  show_count ();
  [%expect {| 0 |}]
;;

let%expect_test "[iter]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  let elts = ref [] in
  for interval_num = 0 to Interval_num.to_int_exn (max_allowed_alarm_interval_num t) do
    elts := add_at_interval_num t ~at:(Interval_num.of_int interval_num) () :: !elts
  done;
  let elts' = ref [] in
  iter t ~f:(fun elt -> elts' := elt :: !elts');
  let sort elts =
    List.sort elts ~compare:(fun elt1 elt2 ->
      Interval_num.compare (Alarm.interval_num t elt1) (Alarm.interval_num t elt2))
  in
  require (List.equal phys_equal (sort !elts) (sort !elts'))
;;

let%expect_test "start after epoch" =
  let t = create_unit ~start:(Time_ns.add Time_ns.epoch (gibi_nanos 1.)) () in
  invariant ignore t
;;

let%expect_test "invalid alarm precision" =
  let test alarm_precision =
    require_does_raise (fun () -> create_unit ~alarm_precision ())
  in
  test (gibi_nanos (-1.));
  [%expect
    {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span"
     (span -1.073741824s))
    |}];
  test (gibi_nanos 0.);
  [%expect
    {| ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span" (span 0s)) |}]
;;

let%expect_test "[Private.interval_num_internal]" =
  for time = -5 to 4 do
    print_s
      [%message
        ""
          (time : int)
          ~interval_num:
            (Interval_num.to_int_exn
               (Private.interval_num_internal
                  ~alarm_precision:
                    (Alarm_precision.of_span_floor_pow2_ns
                       (Time_ns.Span.of_int63_ns (Int63.of_int 4)))
                  ~time:(Time_ns.of_int_ns_since_epoch time))
             : int)]
  done;
  [%expect
    {|
    ((time         -5)
     (interval_num -2))
    ((time         -4)
     (interval_num -1))
    ((time         -3)
     (interval_num -1))
    ((time         -2)
     (interval_num -1))
    ((time         -1)
     (interval_num -1))
    ((time         0)
     (interval_num 0))
    ((time         1)
     (interval_num 0))
    ((time         2)
     (interval_num 0))
    ((time         3)
     (interval_num 0))
    ((time         4)
     (interval_num 1))
    |}]
;;

let%expect_test "[interval_num_start], [interval_start]" =
  let t = create_unit () in
  require (not (mem t (Alarm.null ())));
  let start = start t in
  let test after =
    let time = Time_ns.add start (gibi_nanos after) in
    let interval_num = interval_num t time in
    let interval_num_start = interval_num_start t interval_num in
    let interval_start = interval_start t time in
    print_s
      [%message
        ""
          (interval_num : Interval_num.t)
          (interval_num_start : Time_ns.t)
          (interval_start : Time_ns.t)];
    require (Time_ns.equal interval_num_start interval_start)
  in
  test 0.;
  [%expect
    {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z"))
    |}];
  test 0.1;
  [%expect
    {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z"))
    |}];
  test 0.99;
  [%expect
    {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z"))
    |}];
  test 1.;
  [%expect
    {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z"))
    |}];
  test 1.5;
  [%expect
    {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z"))
    |}];
  test 1.99;
  [%expect
    {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z"))
    |}];
  test 2.;
  [%expect
    {|
    ((interval_num       2)
     (interval_num_start "1970-01-01 00:00:02.147483648Z")
     (interval_start     "1970-01-01 00:00:02.147483648Z"))
    |}]
;;

let%expect_test "[advance_clock]" =
  let t = create_unit () in
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms ()))
    |}];
  let to_ = Time_ns.add (now t) (gibi_nanos 1.) in
  advance_clock t ~to_ ~handle_fired:ignore;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:01.073741824Z")
     (alarms ()))
    |}]
;;

let%expect_test "min alarm at [max_time]" =
  List.iter [ false; true ] ~f:(fun advance_to_max ->
    List.iter [ 1; 2 ] ~f:(fun ns ->
      let alarm_precision = Time_ns.Span.scale_int Time_ns.Span.nanosecond ns in
      let t =
        create_unit ~alarm_precision ~level_bits:[ 1 ] ~extend_to_max_num_bits:true ()
      in
      if advance_to_max then advance_clock t ~to_:max_time ~handle_fired:ignore;
      ignore (add t ~at:max_time () : _ Alarm.t);
      print_s [%message "" (advance_to_max : bool) (ns : int)];
      require_equal
        (module Interval_num)
        (min_alarm_interval_num_exn t)
        (interval_num t max_time)));
  [%expect
    {|
    ((advance_to_max false)
     (ns             1))
    ((advance_to_max false)
     (ns             2))
    ((advance_to_max true)
     (ns             1))
    ((advance_to_max true)
     (ns             2))
    |}]
;;

let%expect_test "[advance_clock ~to_:max_time]" =
  List.iter [ 1; 2; 4 ] ~f:(fun ns ->
    let alarm_precision = Time_ns.Span.scale_int Time_ns.Span.nanosecond ns in
    for level0_bits = 1 to 3 do
      require_does_not_raise ~cr:CR_soon (fun () ->
        let t =
          create_unit
            ~alarm_precision
            ~level_bits:[ level0_bits ]
            ~extend_to_max_num_bits:true
            ()
        in
        print_s [%message "" (alarm_precision : Time_ns.Span.t) (level0_bits : int)];
        for i = 10 downto 0 do
          ignore
            (add
               t
               ~at:(Time_ns.sub max_time (Time_ns.Span.of_int_ns i))
               (fun () -> print_s [%message "alarm" (i : int)])
             : _ Alarm.t)
        done;
        for i = 10 downto 0 do
          print_s [%message "advance" (i : int)];
          advance_clock
            t
            ~to_:(Time_ns.sub max_time (Time_ns.Span.of_int_ns i))
            ~handle_fired:(fun a -> Alarm.value t a ())
        done)
    done);
  [%expect
    {|
    ((alarm_precision 1ns)
     (level0_bits     1))
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
    ((alarm_precision 1ns)
     (level0_bits     2))
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
    ((alarm_precision 1ns)
     (level0_bits     3))
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
    ((alarm_precision 2ns)
     (level0_bits     1))
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
    ((alarm_precision 2ns)
     (level0_bits     2))
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
    ((alarm_precision 2ns)
     (level0_bits     3))
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
    ((alarm_precision 4ns)
     (level0_bits     1))
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
    ((alarm_precision 4ns)
     (level0_bits     2))
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
    ((alarm_precision 4ns)
     (level0_bits     3))
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
    |}]
;;

let%expect_test "[is_empty], [length]" =
  let t = create_unit () in
  let show () =
    print_s [%message "" ~is_empty:(is_empty t : bool) ~length:(length t : int)]
  in
  show ();
  [%expect
    {|
    ((is_empty true)
     (length   0))
    |}];
  let alarm = add t ~at:(now t) () in
  show ();
  [%expect
    {|
    ((is_empty false)
     (length   1))
    |}];
  remove t alarm;
  show ();
  [%expect
    {|
    ((is_empty true)
     (length   0))
    |}]
;;

let%expect_test "[iter]" =
  let t = create_unit () in
  iter t ~f:(fun _ -> require false);
  let alarm1 = add t ~at:(now t) () in
  iter t ~f:(fun alarm -> require (phys_equal alarm alarm1));
  let alarm2 = add t ~at:(now t) () in
  let r = ref 0 in
  iter t ~f:(fun alarm ->
    require (phys_equal alarm alarm1 || phys_equal alarm alarm2);
    incr r);
  print_s [%message (r : int ref)];
  [%expect {| (r 2) |}];
  remove t alarm1;
  remove t alarm2;
  iter t ~f:(fun _ -> require false)
;;

let%expect_test "access to a removed alarm doesn't segfault" =
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ())
      ~start:Time_ns.epoch
  in
  let alarm = add t ~at:(Time_ns.add (now t) (gibi_nanos 5.)) (ref 1) in
  let show_mem () = print_s [%sexp (mem t alarm : bool)] in
  show_mem ();
  [%expect {| true |}];
  remove t alarm;
  show_mem ();
  [%expect {| false |}];
  require_does_raise (fun _ -> Alarm.interval_num t alarm);
  [%expect {| "Timing_wheel got invalid alarm" |}];
  require_does_raise (fun _ -> Alarm.at t alarm);
  [%expect {| "Timing_wheel got invalid alarm" |}];
  require_does_raise (fun _ -> Alarm.value t alarm);
  [%expect {| "Timing_wheel got invalid alarm" |}]
;;

(* Check that [reschedule] and [reschedule_at_interval_num] leave an alarm in the timing
   wheel but reschedule its scheduled time. *)
let test_reschedule reschedule =
  let epoch_plus n_seconds = Time_ns.add Time_ns.epoch (gibi_nanos n_seconds) in
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ~level_bits:[ 10 ] ())
      ~start:(epoch_plus 0.)
  in
  (* add alarm1 before alarm2, test initial conditions *)
  let alarm1 = add t ~at:(epoch_plus 5.) () in
  let alarm2 = add t ~at:(epoch_plus 10.) () in
  let show () =
    let alarm_at alarm = if mem t alarm then Some (Alarm.at t alarm) else None in
    print_s
      [%message
        ""
          ~now:(now t : Time_ns.t)
          ~next_alarm_fires_at:(next_alarm_fires_at t : Time_ns.t option)
          ~alarm1_at:(alarm_at alarm1 : Time_ns.t option)
          ~alarm2_at:(alarm_at alarm2 : Time_ns.t option)]
  in
  show ();
  print_endline "Reschedule alarm1 after alarm2; alarm2 becomes next.";
  reschedule t alarm1 ~at:(epoch_plus 15.);
  show ();
  print_endline "Advance time past alarm1's original time; nothing fires.";
  advance_clock t ~to_:(epoch_plus 7.) ~handle_fired:(fun _ -> require false);
  show ();
  print_endline "Reschedule alarm1 before alarm2 again; alarm1 becomes next.";
  reschedule t alarm1 ~at:(epoch_plus 8.);
  show ();
  print_endline "Advance time past alarm1, alarm1 fires but alarm2 does not.";
  advance_clock t ~to_:(epoch_plus 9.) ~handle_fired:ignore;
  show ();
  print_endline "Cannot reschedule the already-fired alarm1.";
  require_does_raise (fun _ -> reschedule t alarm1 ~at:(epoch_plus 20.));
  show ();
  print_endline "Cannot reschedule before current time.";
  require_does_raise (fun _ -> reschedule t alarm2 ~at:(epoch_plus 8.));
  show ();
  print_endline "Cannot reschedule arbitrarily far in the future.";
  require_does_raise (fun _ ->
    reschedule t alarm2 ~at:(Time_ns.add (max_allowed_alarm_time t) (gibi_nanos 1.)));
  print_endline "Fire alarm2.";
  advance_clock t ~to_:(epoch_plus 11.) ~handle_fired:ignore;
  show ()
;;

let%expect_test "[reschedule]" =
  test_reschedule (fun t alarm ~at -> reschedule t alarm ~at);
  [%expect
    {|
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:06.442450944Z"))
     (alarm1_at ("1970-01-01 00:00:05.36870912Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 after alarm2; alarm2 becomes next.
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1's original time; nothing fires.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 before alarm2 again; alarm1 becomes next.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:09.663676416Z"))
     (alarm1_at ("1970-01-01 00:00:08.589934592Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1, alarm1 fires but alarm2 does not.
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule the already-fired alarm1.
    (Failure "Timing_wheel cannot reschedule alarm not in timing wheel")
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule before current time.
    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at "1970-01-01 00:00:08.589934592Z")
     (now_interval_num_start "1970-01-01 00:00:09.663676416Z"))
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule arbitrarily far in the future.
    ("Timing_wheel cannot schedule alarm that far in the future"
     (at "1970-01-01 00:18:30.249046015Z")
     (max_allowed_alarm_time "1970-01-01 00:18:29.175304191Z"))
    Fire alarm2.
    ((now "1970-01-01 00:00:11.811160064Z")
     (next_alarm_fires_at ())
     (alarm1_at           ())
     (alarm2_at           ()))
    |}]
;;

let%expect_test "[reschedule_at_interval_num]" =
  test_reschedule (fun t alarm ~at ->
    reschedule_at_interval_num t alarm ~at:(interval_num t at));
  [%expect
    {|
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:06.442450944Z"))
     (alarm1_at ("1970-01-01 00:00:05.36870912Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 after alarm2; alarm2 becomes next.
    ((now "1970-01-01 00:00:00Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1's original time; nothing fires.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Reschedule alarm1 before alarm2 again; alarm1 becomes next.
    ((now "1970-01-01 00:00:07.516192768Z")
     (next_alarm_fires_at ("1970-01-01 00:00:09.663676416Z"))
     (alarm1_at ("1970-01-01 00:00:08.589934592Z"))
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Advance time past alarm1, alarm1 fires but alarm2 does not.
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule the already-fired alarm1.
    (Failure "Timing_wheel cannot reschedule alarm not in timing wheel")
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule before current time.
    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at "1970-01-01 00:00:08.589934592Z")
     (now_interval_num_start "1970-01-01 00:00:09.663676416Z"))
    ((now "1970-01-01 00:00:09.663676416Z")
     (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
     (alarm1_at ())
     (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
    Cannot reschedule arbitrarily far in the future.
    ("Timing_wheel cannot schedule alarm that far in the future"
     (at "1970-01-01 00:18:29.175304192Z")
     (max_allowed_alarm_time "1970-01-01 00:18:29.175304191Z"))
    Fire alarm2.
    ((now "1970-01-01 00:00:11.811160064Z")
     (next_alarm_fires_at ())
     (alarm1_at           ())
     (alarm2_at           ()))
    |}]
;;

let%expect_test "[advance_clock] fires alarms at the right time" =
  let test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by =
    let t = create ~config:(create_config ~alarm_precision ()) ~start:Time_ns.epoch in
    for i = 1 to num_alarms do
      let at =
        Time_ns.add (now t) (Time_ns.Span.scale alarm_separation (Float.of_int i))
      in
      ignore (add t ~at (fun () -> require (Time_ns.( <= ) at (now t))) : _ Alarm.t)
    done;
    while not (is_empty t) do
      let to_ = Time_ns.add (now t) advance_by in
      advance_clock t ~to_ ~handle_fired:(fun alarm -> Alarm.value t alarm ());
      require_equal (module Interval_num) (now_interval_num t) (interval_num t to_)
    done
  in
  List.iter
    [ add; (fun t ~at a -> add_at_interval_num t ~at:(interval_num t at) a) ]
    ~f:(fun add ->
      List.iter [ 100 ] ~f:(fun num_alarms ->
        List.iter [ 1.; 0.5; 0.1 ] ~f:(fun s ->
          let alarm_precision = gibi_nanos s in
          List.iter [ 0.01; 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
            let alarm_separation = gibi_nanos s in
            List.iter [ 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
              let advance_by = gibi_nanos s in
              test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by)))))
;;

let%expect_test "[add] and [advance_clock]" =
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ~level_bits:[ 10 ] ())
      ~start:Time_ns.epoch
  in
  let add ~after f = ignore (add t ~at:(Time_ns.add (now t) after) f : _ Alarm.t) in
  let advance_clock by =
    advance_clock
      t
      ~to_:(Time_ns.add (now t) by)
      ~handle_fired:(fun alarm -> Alarm.value t alarm ())
  in
  require_does_raise (fun () -> add ~after:(gibi_nanos (-1.)) ignore);
  [%expect
    {|
    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at "1969-12-31 23:59:58.926258176Z")
     (now_interval_num_start "1970-01-01 00:00:00Z"))
    |}];
  require_equal
    (module Time_ns)
    (Time_ns.add (max_allowed_alarm_time t) Time_ns.Span.nanosecond)
    (Time_ns.add (now t) (gibi_nanos 1024.));
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms ()))
    |}];
  add ~after:(gibi_nanos 30.) ignore;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms ((
       (at    "1970-01-01 00:00:32.21225472Z")
       (value _)))))
    |}];
  advance_clock (gibi_nanos 30.);
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:32.21225472Z")
     (alarms ((
       (at    "1970-01-01 00:00:32.21225472Z")
       (value _)))))
    |}];
  advance_clock (gibi_nanos 1.);
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:33.285996544Z")
     (alarms ()))
    |}]
;;

let%expect_test "[next_alarm_fires_at]" =
  let t = create_unit ~level_bits:[ 10 ] () in
  let next_alarm_fires_after () =
    print_s
      [%message
        ""
          ~next_alarm_fires_after:
            (Option.map (next_alarm_fires_at t) ~f:(fun time ->
               Time_ns.diff time Time_ns.epoch)
             : Time_ns.Span.t option)]
  in
  let add_at at =
    ignore (add t ~at:(Time_ns.add Time_ns.epoch at) () : _ Alarm.t);
    next_alarm_fires_after ()
  in
  let advance_clock span =
    advance_clock t ~to_:(Time_ns.add Time_ns.epoch span) ~handle_fired:ignore;
    next_alarm_fires_after ()
  in
  add_at (gibi_nanos 2.);
  [%expect {| (next_alarm_fires_after (3.221225472s)) |}];
  add_at (gibi_nanos 1.5);
  [%expect {| (next_alarm_fires_after (2.147483648s)) |}];
  add_at (gibi_nanos 1.0);
  [%expect {| (next_alarm_fires_after (2.147483648s)) |}];
  add_at (gibi_nanos 0.5);
  [%expect {| (next_alarm_fires_after (1.073741824s)) |}];
  add_at (gibi_nanos 0.1);
  [%expect {| (next_alarm_fires_after (1.073741824s)) |}];
  advance_clock (gibi_nanos 0.5);
  [%expect {| (next_alarm_fires_after (1.073741824s)) |}];
  advance_clock (gibi_nanos 1.);
  [%expect {| (next_alarm_fires_after (2.147483648s)) |}];
  advance_clock (gibi_nanos 1.5);
  [%expect {| (next_alarm_fires_after (2.147483648s)) |}];
  advance_clock (gibi_nanos 2.);
  [%expect {| (next_alarm_fires_after (3.221225472s)) |}];
  advance_clock (gibi_nanos 3.);
  [%expect {| (next_alarm_fires_after ()) |}]
;;

let%expect_test "[next_alarm_fires_at] with an alarm at [max_time]" =
  let t = create_unit ~level_bits:[ 1 ] ~extend_to_max_num_bits:true () in
  ignore (add t ~at:max_time () : _ Alarm.t);
  print_s [%sexp (next_alarm_fires_at t : Time_ns.t option)];
  [%expect {| () |}];
  require_does_raise (fun () -> next_alarm_fires_at_exn t);
  [%expect
    {|
    ("Timing_wheel.next_alarm_fires_at_exn with all alarms in max interval"
     (timing_wheel (
       (config (
         (alarm_precision 1.073741824s)
         (level_bits (
           1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
       (start            "1970-01-01 00:00:00Z")
       (max_interval_num 4_294_967_295)
       (now              "1970-01-01 00:00:00Z")
       (alarms ((
         (at    "2116-02-20 23:53:38.427387903Z")
         (value _)))))))
    |}]
;;

let%expect_test "[fire_past_alarms] - all possible subsets of alarms in the first bucket \
                 that fire"
  =
  let start = Time_ns.epoch in
  let at sec = Time_ns.add start (gibi_nanos sec) in
  let at1 = at 1. in
  let at2 = at 2. in
  let num_tests = ref 0 in
  for num_elts = 0 to 5 do
    let rec loop i ats =
      incr num_tests;
      if i > 0
      then (
        loop (i - 1) (at1 :: ats);
        loop (i - 1) (at2 :: ats))
      else (
        let t =
          create ~start ~config:(create_config ~alarm_precision:(gibi_nanos 60.) ())
        in
        let num_fired = ref 0 in
        List.iter ats ~f:(fun at ->
          let alarm = add t ~at () in
          require_equal
            (module Interval_num)
            (Alarm.interval_num t alarm)
            Interval_num.zero);
        advance_clock t ~to_:at1 ~handle_fired:(fun _ -> require false);
        fire_past_alarms t ~handle_fired:(fun alarm ->
          if Time_ns.equal (Alarm.at t alarm) at1 then incr num_fired else require false);
        require_equal (module Int) !num_fired (List.count ats ~f:(Time_ns.equal at1)))
    in
    loop num_elts []
  done;
  print_s [%message (num_tests : int ref)];
  [%expect {| (num_tests 120) |}]
;;

let%expect_test "alarm buckets" =
  let start = Time_ns.epoch in
  let t : bool ref t =
    create ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ()) ~start
  in
  let handle_fired (a : bool ref Alarm.t) : unit =
    let r = Alarm.value t a in
    require (not !r);
    r := true
  in
  let precision = alarm_precision t in
  let precision_0_2 = Time_ns.Span.scale precision 0.2 in
  let _ = add t ~at:(Time_ns.add start precision) (ref false) in
  let base = next_alarm_fires_at t |> Option.value_exn in
  let step0 = Time_ns.add base precision_0_2 in
  let step1 = Time_ns.add step0 precision_0_2 in
  let step2 = Time_ns.add step1 precision_0_2 in
  let step3 = Time_ns.add step2 precision in
  (* Check all alarm will be in the same bucket but step3 *)
  let interval_num0 = interval_num t step0 in
  let interval_num1 = interval_num t step1 in
  let interval_num2 = interval_num t step2 in
  let interval_num3 = interval_num t step3 in
  print_s
    [%message
      ""
        (interval_num0 : Interval_num.t)
        (interval_num1 : Interval_num.t)
        (interval_num2 : Interval_num.t)
        (interval_num3 : Interval_num.t)];
  [%expect
    {|
    ((interval_num0 2)
     (interval_num1 2)
     (interval_num2 2)
     (interval_num3 3))
    |}];
  let step1_fired = ref false in
  let step2_fired = ref false in
  let step3_fired = ref false in
  let _ = add t ~at:step1 step1_fired in
  let _ = add t ~at:step2 step2_fired in
  let _ = add t ~at:step3 step3_fired in
  let show () =
    print_s
      [%message
        "" (step1_fired : bool ref) (step2_fired : bool ref) (step3_fired : bool ref)]
  in
  (* Nothing should be triggered before *)
  advance_clock t ~to_:step0 ~handle_fired;
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false))
    |}];
  advance_clock t ~to_:step1 ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false))
    |}];
  show ();
  [%expect
    {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false))
    |}];
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired true)
     (step2_fired false)
     (step3_fired false))
    |}];
  advance_clock t ~to_:step2 ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired true)
     (step2_fired false)
     (step3_fired false))
    |}];
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired true)
     (step2_fired true)
     (step3_fired false))
    |}]
;;

let%expect_test "[max_alarm_time_in_min_interval]" =
  let t = create_unit () ~level_bits:[ 1; 1 ] in
  let max_alarm_time_in_min_interval () =
    print_s [%sexp (max_alarm_time_in_min_interval t : Time_ns.t option)]
  in
  max_alarm_time_in_min_interval ();
  [%expect {| () |}];
  let add_after span = add t ~at:(Time_ns.add (now t) span) ignore in
  let a = add_after (gibi_nanos 0.5) in
  max_alarm_time_in_min_interval ();
  [%expect {| ("1970-01-01 00:00:00.536870912Z") |}];
  remove t a;
  max_alarm_time_in_min_interval ();
  [%expect {| () |}];
  (* Add two alarms that appear in different intervals, but in the same slot on the
     second level of the timing wheel.  So the correct [max_alarm_time_in_min_interval] is
     2.1, not 3.9. *)
  let _ = add_after (gibi_nanos 2.1) in
  let _ = add_after (gibi_nanos 3.9) in
  max_alarm_time_in_min_interval ();
  [%expect {| ("1970-01-01 00:00:02.25485783Z") |}]
;;

let%expect_test "[min_alarm_time_in_min_interval]" =
  let t = create_unit () ~level_bits:[ 1; 1 ] in
  let min_alarm_time_in_min_interval () =
    print_s [%sexp (min_alarm_time_in_min_interval t : Time_ns.t option)]
  in
  min_alarm_time_in_min_interval ();
  [%expect {| () |}];
  let add_after span = add t ~at:(Time_ns.add (now t) span) ignore in
  let a = add_after (gibi_nanos 0.5) in
  min_alarm_time_in_min_interval ();
  [%expect {| ("1970-01-01 00:00:00.536870912Z") |}];
  remove t a;
  min_alarm_time_in_min_interval ();
  [%expect {| () |}];
  let _ = add_after (gibi_nanos 2.1) in
  let _ = add_after (gibi_nanos 3.9) in
  min_alarm_time_in_min_interval ();
  [%expect {| ("1970-01-01 00:00:02.25485783Z") |}]
;;

let%expect_test "multiple alarms at the same time are fired in insertion order" =
  let t = create_unit () in
  let delta = Time_ns.Span.of_sec 1. in
  let at = Time_ns.(add epoch delta) in
  for i = 0 to 5 do
    ignore (add t ~at i)
  done;
  advance_clock t ~to_:(Time_ns.add at delta) ~handle_fired:(fun alarm ->
    print_s [%sexp (Alarm.value t alarm : int)]);
  [%expect
    {|
    0
    1
    2
    3
    4
    5
    |}]
;;

let%expect_test "max_alarm_time can not be exceeded by [add] or [add_at_interval_num]" =
  let t = create_unit ~level_bits:[ 10 ] () in
  let succ t =
    let r = Time_ns.add t Time_ns.Span.nanosecond in
    assert (Time_ns.( > ) r t);
    r
  in
  let bad_time = succ (max_allowed_alarm_time t) in
  require_does_raise (fun () -> add t ~at:bad_time ());
  [%expect
    {|
    ("Timing_wheel cannot schedule alarm that far in the future"
     (at "1970-01-01 00:18:19.511627776Z")
     (max_allowed_alarm_time "1970-01-01 00:18:19.511627775Z"))
    |}];
  require_does_raise (fun () ->
    add_at_interval_num
      t
      ~at:(Interval_num.succ (interval_num t (max_allowed_alarm_time t)))
      ());
  [%expect
    {|
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   1_024)
     (min_allowed_alarm_interval_num 0)
     (max_allowed_alarm_interval_num 1_023))
    |}]
;;

*)
