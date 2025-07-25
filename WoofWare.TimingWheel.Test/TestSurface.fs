namespace WoofWare.TimingWheel.Test

open NUnit.Framework
open ApiSurface

[<TestFixture>]
module TestSurface =
    let assembly = typeof<WoofWare.TimingWheel.TimingWheel<obj>>.Assembly

    [<Test>]
    let ``Ensure API surface has not been modified`` () = ApiSurface.assertIdentical assembly

    [<Test ; Explicit>]
    let ``Update API surface`` () =
        ApiSurface.writeAssemblyBaseline assembly

    [<Test ; Explicit "TODO: document the library">]
    let ``Ensure public API is fully documented`` () =
        DocCoverage.assertFullyDocumented assembly

    [<Test>]
    // https://github.com/nunit/nunit3-vs-adapter/issues/876
    let ``EnsureVersionIsMonotonic`` () =
        MonotonicVersion.validate assembly "WoofWare.TimingWheel"
