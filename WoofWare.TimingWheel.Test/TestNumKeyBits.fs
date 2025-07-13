namespace WoofWare.TimingWheel.Test

open NUnit.Framework
open WoofWare.TimingWheel

[<TestFixture>]
module TestNumKeyBits =

    [<Test>]
    let ``zero invariant`` () = NumKeyBits.invariant NumKeyBits.zero
