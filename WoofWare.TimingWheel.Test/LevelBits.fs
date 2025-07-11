namespace WoofWare.TimingWheel.Test

open WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module LevelBits =

    let display (lb : LevelBits) : string =
        lb
        |> List.map string<int>
        |> String.concat ", "
        |> sprintf "(%s)"
