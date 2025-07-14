namespace WoofWare.TimingWheel.Test

open System
open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module TimeNs =
    let format (t : TimeNs) : string =
        let ts = TimeSpan.FromMicroseconds (float<int64<_>> t / 1000.0)
        let dt = DateTime.UnixEpoch + ts
        dt.ToString "O"
