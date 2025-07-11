namespace WoofWare.TimingWheel.Test

open NUnit.Framework
open WoofWare.TimingWheel

[<TestFixture>]
module TestConfig =

let%expect_test "[Config.microsecond_precision]" =
  print_s [%sexp (Config.microsecond_precision () : Config.t)];
  [%expect {| ((alarm_precision 1.024us) (level_bits (10 10 6 6 5))) |}];
  print_s
    [%sexp (Config.durations (Config.microsecond_precision ()) : Time_ns.Span.t list)];
  [%expect
    {|
    (1.048576ms
     1.073741824s
     1m8.719476736s
     1h13m18.046511104s
     1d15h5m37.488355328s)
    |}]
;;

let create_config ?extend_to_max_num_bits ?level_bits ~alarm_precision () =
  Config.create
    ()
    ~alarm_precision:(alarm_precision |> Alarm_precision.of_span_floor_pow2_ns)
    ?level_bits:(Option.map level_bits ~f:(Level_bits.create_exn ?extend_to_max_num_bits))
;;

let%expect_test "[Config.create] with negative alarm precision" =
  require_does_raise (fun () -> create_config ~alarm_precision:(gibi_nanos (-1.)) ());
  [%expect
    {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span"
     (span -1.073741824s))
    |}]
;;

let%expect_test "[Config.create] with zero alarm precision" =
  require_does_raise (fun () -> create_config ~alarm_precision:(gibi_nanos 0.) ());
  [%expect
    {| ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span" (span 0s)) |}]
;;

let%expect_test "[Config.create] with one second alarm precision" =
  print_s [%sexp (create_config ~alarm_precision:(gibi_nanos 1.) () : Config.t)];
  [%expect {| ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))) |}]
;;

let%expect_test "[Config.durations]" =
  let durations ?extend_to_max_num_bits level_bits =
    print_s
      [%sexp
        (Config.durations
           (create_config
              ?extend_to_max_num_bits
              ~alarm_precision:(gibi_nanos 1.)
              ~level_bits
              ())
         : Time_ns.Span.t list)]
  in
  durations [ 1 ];
  [%expect {| (2.147483648s) |}];
  durations [ 2; 1 ];
  [%expect {| (4.294967296s 8.589934592s) |}];
  durations (List.init 32 ~f:(const 1));
  [%expect
    {|
    (2.147483648s
     4.294967296s
     8.589934592s
     17.179869184s
     34.359738368s
     1m8.719476736s
     2m17.438953472s
     4m34.877906944s
     9m9.755813888s
     18m19.511627776s
     36m39.023255552s
     1h13m18.046511104s
     2h26m36.093022208s
     4h53m12.186044416s
     9h46m24.372088832s
     19h32m48.744177664s
     1d15h5m37.488355328s
     3d6h11m14.976710656s
     6d12h22m29.953421312s
     13d44m59.906842624s
     26d1h29m59.813685248s
     52d2h59m59.627370496s
     104d5h59m59.254740992s
     208d11h59m58.509481984s
     416d23h59m57.018963968s
     833d23h59m54.037927936s
     1667d23h59m48.075855872s
     3335d23h59m36.151711744s
     6671d23h59m12.303423488s
     13343d23h58m24.606846976s
     26687d23h56m49.213693952s
     53375d23h53m38.427387903s)
    |}];
  durations [ 10; 10; 10 ] ~extend_to_max_num_bits:true;
  [%expect
    {|
    (18m19.511627776s
     13d44m59.906842624s
     13343d23h58m24.606846976s
     26687d23h56m49.213693952s
     53375d23h53m38.427387903s)
    |}]
;;

