namespace WoofWare.TimingWheel.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel

[<TestFixture>]
module TestAlarmPrecision =

    [<Test>]
    let ``constant values`` () =
        expect {
            snapshot ""
            return AlarmPrecision.aboutOneDay
        }
        expect {
            snapshot ""
            return AlarmPrecision.aboutOneSecond
        }
        expect {
            snapshot ""
            return AlarmPrecision.aboutOneMicrosecond
        }
        expect {
            snapshot ""
            return AlarmPrecision.aboutOneMillisecond
        }
        expect {
            snapshot ""
            return AlarmPrecision.oneNanosecond
        }

    (*
    print about_one_day;
    [%expect {| (19h32m48.744177664s 70_368_744_177_664ns) |}];
    print about_one_second;
    [%expect {| (1.073741824s 1_073_741_824ns) |}];
    print about_one_microsecond;
    [%expect {| (1.024us 1_024ns) |}];
    print about_one_millisecond;
    [%expect {| (1.048576ms 1_048_576ns) |}];
    print one_nanosecond;
    [%expect {| (1ns 1ns) |}]
    *)

  let%expect_test "[div]" =
    for pow2 = -3 to 3 do
      print (div about_one_second ~pow2)
    done;
    [%expect
      {|
      (8.589934592s 8_589_934_592ns)
      (4.294967296s 4_294_967_296ns)
      (2.147483648s 2_147_483_648ns)
      (1.073741824s 1_073_741_824ns)
      (536.870912ms 536_870_912ns)
      (268.435456ms 268_435_456ns)
      (134.217728ms 134_217_728ns)
      |}]
  ;;

  let%expect_test "[mul]" =
    for pow2 = -3 to 3 do
      print (mul about_one_second ~pow2)
    done;
    [%expect
      {|
      (134.217728ms 134_217_728ns)
      (268.435456ms 268_435_456ns)
      (536.870912ms 536_870_912ns)
      (1.073741824s 1_073_741_824ns)
      (2.147483648s 2_147_483_648ns)
      (4.294967296s 4_294_967_296ns)
      (8.589934592s 8_589_934_592ns)
      |}]
  ;;

  let%expect_test "[of_span_floor_pow2_ns]" =
    List.iter
      [ about_one_day
      ; about_one_second
      ; about_one_millisecond
      ; about_one_microsecond
      ; one_nanosecond
      ]
      ~f:(fun t ->
        require (equal t (t |> to_span |> of_span_floor_pow2_ns));
        if Time_ns.Span.( > ) (t |> to_span) Time_ns.Span.nanosecond
        then
          require
            (equal
               t
               (Time_ns.Span.( + ) (t |> to_span) Time_ns.Span.nanosecond
                |> of_span_floor_pow2_ns)));
    List.iter [ 1.; 1E-3; 1E-6 ] ~f:(fun span ->
      let span = Time_ns.Span.of_sec span in
      print_s
        [%message
          "" (span : Time_ns.Span.t) ~alarm_precision:(span |> of_span_floor_pow2_ns : t)]);
    [%expect
      {|
      ((span 1s) (alarm_precision (536.870912ms 536_870_912ns)))
      ((span 1ms) (alarm_precision (524.288us 524_288ns)))
      ((span 1us) (alarm_precision (512ns 512ns)))
      |}]
  ;;
