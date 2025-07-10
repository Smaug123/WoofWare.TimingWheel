namespace WoofWare.TimingWheel

type EltPointer = int voption

// This is the public type!
type ExternalEltValue<'a> =
    {
        mutable Key : Key
        mutable AtTime : TimeNs
        mutable Value : 'a
        mutable LevelIndex : int
        mutable Prev : EltPointer
        mutable Next : EltPointer
    }

type ExternalElt = EltPointer
