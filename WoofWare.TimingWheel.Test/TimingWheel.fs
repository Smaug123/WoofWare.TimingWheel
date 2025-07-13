namespace WoofWare.TimingWheel.Test

open System.Text
open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module TimingWheel =
    let display (t : TimingWheel<ExternalEltValue<'a>>) =
        let config = Config.display t.Config
        let start = TimeNs.format t.Start
        let maxIntervalNum = string<int64> t.MaxIntervalNum
        let now = TimeNs.format t.Now

        let alarms =
            let result = StringBuilder ()

            TimingWheel.iter
                t
                (fun elt ->
                    result.Append(TimeNs.format (TimingWheel.Alarm.atTime t elt)).Append ('\n')
                    |> ignore<StringBuilder>
                )

            result.ToString ()

        $"config: %s{config}\nstart: %s{start}\nmaxIntervalNum: %s{maxIntervalNum}\nnow: %s{now}\nalarms:\n%s{alarms}"
