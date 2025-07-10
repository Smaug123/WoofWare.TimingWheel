namespace WoofWare.TimingWheel

[<RequireQualifiedAccess>]
module internal Int64 =
    let floorLog2 (i : int64) : int =
        if i <= 0L then
            invalidArg "x" "Input must be positive"
        else
            63 - System.Numerics.BitOperations.LeadingZeroCount(uint64 i)

