namespace WoofWare.TimingWheel.Test

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module Span =
    let display (t : TimeNs.Span) : string =
        let ts = System.TimeSpan.FromMicroseconds (float t / 1000.0)
        let seconds = $"%02d{ts.Seconds}s.%09.0f{(ts.TotalSeconds % 1.0) * 1_000_000_000.0}"

        let minutes =
            if ts.TotalMinutes >= 1.0 then
                $"%02d{ts.Minutes}m%s{seconds}"
            else
                seconds

        let hours =
            if ts.TotalHours >= 1.0 then
                $"%02d{ts.Hours}h%s{minutes}"
            else
                minutes

        if ts.TotalDays >= 1.0 then
            $"%02d{ts.Days}d%s{hours}"
        else
            hours
