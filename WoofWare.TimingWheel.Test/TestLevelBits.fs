namespace WoofWare.TimingWheel.Test

open WoofWare.TimingWheel
open NUnit.Framework

[<TestFixture>]
module TestLevelBits =


let%expect_test _ =
  print_s [%sexp (Level_bits.max_num_bits : int)];
  [%expect {| 62 |}]
;;

let%expect_test "invalid level bits" =
  let test level_bits =
    require_does_raise (fun () -> Level_bits.create_exn level_bits);
    require_does_raise ~hide_positions:true (fun () ->
      [%of_sexp: Level_bits.t] ([%sexp_of: int list] level_bits))
  in
  test [];
  [%expect
    {|
    (Failure "Level_bits.create_exn requires a nonempty list")
    "Assert_failure timing_wheel.ml:LINE:COL"
    |}];
  test [ 0 ];
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits" (0))
    "Assert_failure timing_wheel.ml:LINE:COL"
    |}];
  test [ -1 ];
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits" (-1))
    "Assert_failure timing_wheel.ml:LINE:COL"
    |}];
  test [ 2; 0; 1 ];
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits" (2 0 1))
    "Assert_failure timing_wheel.ml:LINE:COL"
    |}];
  test [ Level_bits.max_num_bits + 1 ];
  [%expect
    {|
    ("Level_bits.create_exn got too many bits"
      (63)
      (got          63)
      (max_num_bits 62))
    "Assert_failure timing_wheel.ml:LINE:COL"
    |}];
  test (List.init (Level_bits.max_num_bits + 1) ~f:Fn.id);
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits"
     (0
      1
      2
      3
      4
      5
      6
      7
      8
      9
      10
      11
      12
      13
      14
      15
      16
      17
      18
      19
      20
      21
      22
      23
      24
      25
      26
      27
      28
      29
      30
      31
      32
      33
      34
      35
      36
      37
      38
      39
      40
      41
      42
      43
      44
      45
      46
      47
      48
      49
      50
      51
      52
      53
      54
      55
      56
      57
      58
      59
      60
      61
      62))
    "Assert_failure timing_wheel.ml:LINE:COL"
    |}]
;;

let%expect_test _ = Level_bits.invariant Level_bits.default

let%expect_test "[Level_bits.num_bits]" =
  let num_bits bits =
    let level_bits = Level_bits.create_exn bits in
    print_s [%sexp (Level_bits.num_bits level_bits : int)];
    let sexp = [%sexp (level_bits : Level_bits.t)] in
    require_equal
      (module Sexp)
      sexp
      (sexp |> [%of_sexp: Level_bits.t] |> [%sexp_of: Level_bits.t])
  in
  num_bits [ 1 ];
  [%expect {| 1 |}];
  num_bits [ 1; 1 ];
  [%expect {| 2 |}];
  num_bits [ 1; 2; 3 ];
  [%expect {| 6 |}]
;;

