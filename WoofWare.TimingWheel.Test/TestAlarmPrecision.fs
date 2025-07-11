namespace WoofWare.TimingWheel.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel
open FsUnitTyped

[<TestFixture>]
module TestAlarmPrecision =
    [<OneTimeSetUp>]
    let oneTimeSetUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let oneTimeTearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``constant values`` () =
        expect {
            snapshot @"19h32m48s.744177600 (70368744177664 ns)"
            return AlarmPrecision.aboutOneDay |> AlarmPrecision.display
        }
        expect {
            snapshot @"01s.073741800 (1073741824 ns)"
            return AlarmPrecision.aboutOneSecond |> AlarmPrecision.display
        }
        expect {
            snapshot @"00s.000001000 (1024 ns)"
            return AlarmPrecision.aboutOneMicrosecond |> AlarmPrecision.display
        }
        expect {
            snapshot @"00s.001048500 (1048576 ns)"
            return AlarmPrecision.aboutOneMillisecond |> AlarmPrecision.display
        }
        expect {
            snapshot @"00s.000000000 (1 ns)"
            return AlarmPrecision.oneNanosecond |> AlarmPrecision.display
        }

    [<Test>]
    let ``test div`` () =
        expect {
            snapshotJson @"[
  ""08s.589934500 (8589934592 ns)"",
  ""04s.294967200 (4294967296 ns)"",
  ""02s.147483600 (2147483648 ns)"",
  ""01s.073741800 (1073741824 ns)"",
  ""00s.536870900 (536870912 ns)"",
  ""00s.268435400 (268435456 ns)"",
  ""00s.134217700 (134217728 ns)""
]"
            return [-3 .. 3] |> List.map (AlarmPrecision.div AlarmPrecision.aboutOneSecond >> AlarmPrecision.display)
        }

    [<Test>]
    let ``test mul`` () =
        expect {
            snapshotJson @"[
  ""00s.134217700 (134217728 ns)"",
  ""00s.268435400 (268435456 ns)"",
  ""00s.536870900 (536870912 ns)"",
  ""01s.073741800 (1073741824 ns)"",
  ""02s.147483600 (2147483648 ns)"",
  ""04s.294967200 (4294967296 ns)"",
  ""08s.589934500 (8589934592 ns)""
]"
            return [-3 .. 3] |> List.map (AlarmPrecision.mul AlarmPrecision.aboutOneSecond >> AlarmPrecision.display)
        }

    [<Test>]
    let ``test ofSpanFloorPow2Ns`` () : unit =
        for t in [ AlarmPrecision.aboutOneDay ; AlarmPrecision.aboutOneSecond ; AlarmPrecision.aboutOneMillisecond ; AlarmPrecision.aboutOneMicrosecond ; AlarmPrecision.oneNanosecond ] do
            t
            |> shouldEqual (t |> AlarmPrecision.toSpan |> AlarmPrecision.ofSpanFloorPow2Ns)

            if AlarmPrecision.toSpan t > TimeNs.Span.nanosecond then
                t
                |> shouldEqual ((AlarmPrecision.toSpan t + TimeNs.Span.nanosecond) |> AlarmPrecision.ofSpanFloorPow2Ns)

        expect {
            snapshotJson @"[
  [
    ""01s.000000000"",
    ""00s.536870900 (536870912 ns)""
  ],
  [
    ""00s.001000000"",
    ""00s.000524200 (524288 ns)""
  ],
  [
    ""00s.000001000"",
    ""00s.000000500 (512 ns)""
  ]
]"
            return
                [1.0;  1e-3 ; 1e-6]
                |> List.map (fun s ->
                    let span = TimeNs.Span.ofSec s
                    Span.display span, AlarmPrecision.display (AlarmPrecision.ofSpanFloorPow2Ns span)
                )
        }
