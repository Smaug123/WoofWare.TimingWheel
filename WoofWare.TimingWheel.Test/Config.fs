namespace WoofWare.TimingWheel.Test

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module Config =

    let display (c : Config) : string =
        $"%s{AlarmPrecision.display c.AlarmPrecision} : %s{LevelBits.display c.LevelBits}"
