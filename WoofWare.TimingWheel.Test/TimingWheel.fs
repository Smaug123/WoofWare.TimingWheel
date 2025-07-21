namespace WoofWare.TimingWheel.Test

open System.Text
open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module TimingWheel =
    let display (t : TimingWheel<ExternalEltValue<'a>>) =
        let config = TimingWheelConfig.display t.Config
        let start = TimeNs.display (TimingWheel.start t)
        let maxIntervalNum = string<int64> (TimingWheel.maxIntervalNum t)
        let now = TimeNs.display (TimingWheel.now t)

        let alarms =
            let result = StringBuilder ()

            TimingWheel.iter
                t
                (fun elt ->
                    result.Append(TimeNs.display (TimingWheel.Alarm.atTime t elt)).Append ('\n')
                    |> ignore<StringBuilder>
                )

            result.ToString ()

        $"config: %s{config}\nstart: %s{start}\nmaxIntervalNum: %s{maxIntervalNum}\nnow: %s{now}\nalarms:\n%s{alarms}"
