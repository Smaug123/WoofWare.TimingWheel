namespace WoofWare.TimingWheel.Test

open NUnit.Framework
open WoofWare.Expect
open WoofWare.TimingWheel

[<TestFixture>]
module TestConfig =

    [<OneTimeSetUp>]
    let oneTimeSetUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let oneTimeTearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``microsecondPrecision test`` () =
        expect {
            snapshot @"00s.000001000 (1024 ns) : (10, 10, 6, 6, 5)"
            return Config.microsecondPrecision () |> Config.display
        }

        expect {
            snapshotJson @"[
  ""00s.001048500"",
  ""01s.073741800"",
  ""01m08s.719476700"",
  ""01h13m18s.046511100"",
  ""01d15h05m37s.488355300""
]"
            return Config.durations (Config.microsecondPrecision ()) |> List.map Span.display
        }

    /// giga-nanoseecond
    let gibi = pown 2.0 30
    let gibiNanos float = float * gibi |> System.Math.Round |> int64<float> |> TimeNs.Span.ofInt64Ns

    let createConfig (extendToMaxNumBits: bool option) (levelBits: int list option) (alarmPrecision: TimeNs.Span) : Config =
        Config.create None (levelBits |> Option.map (fun l ->
            match extendToMaxNumBits with
            | None ->LevelBits.createThrowing l
            | Some extendToMaxNumBits -> LevelBits.createThrowing' extendToMaxNumBits l
        ) |> Option.defaultValue LevelBits.default') (alarmPrecision |> AlarmPrecision.ofSpanFloorPow2Ns)

    [<Test>]
    let ``create with negative alarm precision`` () =
        expect {
            snapshotThrows @"System.ArgumentException: expected positive span (Parameter 'span')"
            return! (fun () -> createConfig None None (gibiNanos -1.0))
        }

    [<Test>]
    let ``create with zero alarm precision`` () =
        expect {
            snapshotThrows @"System.ArgumentException: expected positive span (Parameter 'span')"
            return! (fun () -> createConfig None None (gibiNanos 0.0))
        }

    [<Test>]
    let ``create with one second alarm precision`` () =
        expect {
            // TODO: this is wrong!
            snapshot @"01s.073741800 (1073741824 ns) : (11, 10, 10, 1)"
            return createConfig None None (gibiNanos 1.0) |> Config.display
        }

    [<Test>]
    let ``Config durations test`` () : unit =
        let durations extendToMaxNumBits (levelBits : LevelBits) =
            Config.durations (createConfig extendToMaxNumBits (Some levelBits) (gibiNanos 1.0))

        expect {
            snapshot @"[02s.147483600]"
            return durations None [ 1 ] |> List.map Span.display
        }

        expect {
            snapshot @"[04s.294967200; 08s.589934500]"
            return durations None [ 2 ; 1 ] |> List.map Span.display
        }

        expect {
            snapshotJson @"[
  ""02s.147483600"",
  ""04s.294967200"",
  ""08s.589934500"",
  ""17s.179869100"",
  ""34s.359738300"",
  ""01m08s.719476700"",
  ""02m17s.438953400"",
  ""04m34s.877906900"",
  ""09m09s.755813800"",
  ""18m19s.511627700"",
  ""36m39s.023255500"",
  ""01h13m18s.046511100"",
  ""02h26m36s.093022200"",
  ""04h53m12s.186044400"",
  ""09h46m24s.372088800"",
  ""19h32m48s.744177600"",
  ""01d15h05m37s.488355300"",
  ""03d06h11m14s.976710600"",
  ""06d12h22m29s.953421300"",
  ""13d00h44m59s.906842600"",
  ""26d01h29m59s.813685200"",
  ""52d02h59m59s.627370400"",
  ""104d05h59m59s.254740899"",
  ""208d11h59m58s.509481899"",
  ""416d23h59m57s.018963903"",
  ""833d23h59m54s.037927896"",
  ""1667d23h59m48s.075855792"",
  ""3335d23h59m36s.151711702"",
  ""6671d23h59m12s.303423524"",
  ""13343d23h58m24s.606847048"",
  ""26687d23h56m49s.213694096"",
  ""53375d23h53m38s.427388191""
]"
            return durations None (List.replicate 32 1) |> List.map Span.display
        }

        expect {
            snapshotJson @"[
  ""18m19s.511627700"",
  ""13d00h44m59s.906842600"",
  ""13343d23h58m24s.606847048"",
  ""26687d23h56m49s.213694096"",
  ""53375d23h53m38s.427388191"",
  ""106751d23h47m16s.854776382""
]"
            return durations (Some true) [ 10;10;10 ] |> List.map Span.display
        }
        failwith "TODO: we have an extra entry there for some reason"
