namespace WoofWare.TimingWheel.Test

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module AlarmPrecision =

    let display (t : AlarmPrecision) : string =
        let span =
            t
            |> AlarmPrecision.toSpan
        let ns =
            span
            |> TimeNs.Span.toInt64Ns
            |> sprintf "%i ns"
        let span =
            Span.display span

        $"%s{span} (%s{ns})"

