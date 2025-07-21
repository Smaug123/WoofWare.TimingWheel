namespace WoofWare.TimingWheel.Test

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module TimingWheelConfig =

    let display (c : TimingWheelConfig) : string =
        $"%s{AlarmPrecision.display c.AlarmPrecision} : %s{LevelBits.display c.LevelBits}"
