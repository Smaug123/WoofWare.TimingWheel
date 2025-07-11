namespace WoofWare.TimingWheel.Test

open WoofWare.Expect
open WoofWare.TimingWheel
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestLevelBits =
    [<OneTimeSetUp>]
    let oneTimeSetUp () =
        // GlobalBuilderConfig.enterBulkUpdateMode ()
        ()

    [<OneTimeTearDown>]
    let oneTimeTearDown () =
        GlobalBuilderConfig.updateAllSnapshots ()

    [<Test>]
    let ``maxNumBits test`` () =
        // We have one more bit than OCaml does
        LevelBits.maxNumBits |> shouldEqual 63

    [<Test>]
    let ``invalid level bits`` () =
        expect {
            snapshotThrows @"System.ArgumentException: expected nonempty list (Parameter 'ints')"
            return! (fun () -> LevelBits.createThrowing [])
        }

        expect {
            snapshotThrows @"System.ArgumentException: expected positive num bits (Parameter 'ints')"
            return! (fun () -> LevelBits.createThrowing [ 0 ])
        }

        expect {
            snapshotThrows @"System.ArgumentException: expected positive num bits (Parameter 'ints')"
            return! (fun () -> LevelBits.createThrowing [ -1 ])
        }

        expect {
            snapshotThrows @"System.ArgumentException: expected positive num bits (Parameter 'ints')"
            return! (fun () -> LevelBits.createThrowing [ 2 ; 0 ; 1 ])
        }

        expect {
            snapshotThrows @"System.ArgumentException: too many bits: 64, more than 64 (Parameter 'ints')"
            return! (fun () -> LevelBits.createThrowing [ LevelBits.maxNumBits + 1 ])
        }

        expect {
            snapshotThrows @"System.ArgumentException: expected positive num bits (Parameter 'ints')"
            return! (fun () -> LevelBits.createThrowing [ 0 .. LevelBits.maxNumBits ])
        }

    [<Test>]
    let ``Test numBits`` () =
        let numBits bits =
            let levelBits = LevelBits.createThrowing bits
            LevelBits.numBits levelBits

        numBits [ 1 ] |> shouldEqual 1
        numBits [ 1 ; 1 ] |> shouldEqual 2
        numBits [ 1 ; 2 ; 3 ] |> shouldEqual 6

    [<Test>]
    let ``Test invariant`` () = LevelBits.invariant LevelBits.default'
