namespace WoofWare.TimingWheel

type TimingWheel<'a> =
    {
        Config : Config
        Start : TimeNs
        MaxIntervalNum : IntervalNum
        mutable Now : TimeNs
        mutable NowIntervalNumStart : TimeNs
        mutable MaxAllowedAlarmTime : TimeNs
        PriorityQueue : PriorityQueue<'a>
    }

[<RequireQualifiedAccess>]
module TimingWheel =

    let alarmPrecision (t : TimingWheel<'a>) : TimeNs.Span = Config.alarmPrecision t.Config

    type Alarm = Elt

    module Alarm =
        let null' : Alarm = Elt.null'
        let atTime (tw : TimingWheel<ExternalEltValue<'a>>) (t : ExternalElt) : TimeNs = Elt.at tw.PriorityQueue t
        let value (tw : TimingWheel<ExternalEltValue<'a>>) (t : ExternalElt) : 'a = Elt.value tw.PriorityQueue t
        let intervalNum (tw : TimingWheel<ExternalEltValue<'a>>) (t : ExternalElt) : Key = Elt.key tw.PriorityQueue t


    let iter (t : TimingWheel<ExternalEltValue<'a>>) (f : Elt -> unit) : unit = PriorityQueue.iter t.PriorityQueue f

    let length (t : TimingWheel<'a>) : int = t.PriorityQueue.Length
    let isEmpty (t : TimingWheel<'a>) : bool = length t = 0

    let raiseNextAlarmFiresAtThrowingOfEmptyTimingWheel () =
        failwith "nextAlarmFiresAtThrowing of empty timing wheel"

    let raiseNextAlarmFiresAtWithAllAlarmsInMaxInterval () =
        failwith "nextAlarmFiresAtThrowing with all alarms in max interval"

    let internal pool (t : TimingWheel<'a>) : Pool<'a> = t.PriorityQueue.Pool

    let intervalNumInternal (time : TimeNs) (alarmPrecision : AlarmPrecision) : IntervalNum =
        IntervalNum.ofInt64 (AlarmPrecision.intervalNum alarmPrecision time)

    let intervalNumUnchecked (t : TimingWheel<'a>) (time : TimeNs) : IntervalNum =
        intervalNumInternal time t.Config.AlarmPrecision

    let minTime = TimeNs.epoch
    let maxTime : TimeNs = TimeNs.maxValueRepresentable
    let minIntervalNum = IntervalNum.zero

    let intervalNum (t : TimingWheel<'a>) (time : TimeNs) : IntervalNum =
        if time < minTime then
            failwith $"intervalNum got time too far in the past: {time}, min is {minTime}"

        intervalNumUnchecked t time

    let intervalNumStartUnchecked (t : TimingWheel<'a>) (intervalNum : IntervalNum) : TimeNs =
        AlarmPrecision.intervalNumStart t.Config.AlarmPrecision (intervalNum |> IntervalNum.toInt64)

    let raiseIntervalNumStartGotTooSmall (intervalNum : IntervalNum) : unit =
        failwith $"intervalNumStart got too small intervalNum: {intervalNum}, min was {minIntervalNum}"

    let raiseIntervalNumStartGotTooLarge (t : TimingWheel<'a>) (intervalNum : IntervalNum) : unit =
        failwith $"intervalNumStart got too large intervalNum: {intervalNum}, max was {t.MaxIntervalNum}"

    let intervalNumStart (t : TimingWheel<'a>) (intervalNum : IntervalNum) : TimeNs =
        if intervalNum < minIntervalNum then
            raiseIntervalNumStartGotTooSmall intervalNum

        if intervalNum > t.MaxIntervalNum then
            raiseIntervalNumStartGotTooLarge t intervalNum

        intervalNumStartUnchecked t intervalNum

    let nextAlarmFiresAtInternal (t : TimingWheel<'a>) (key : Key) : TimeNs =
        (* [interval_num_start t key] is the key corresponding to the start of the time interval
     holding the first alarm in [t].  Advancing to that would not be enough, since the
     alarms in that interval don't fire until the clock is advanced to the start of the
     next interval.  So, we use [succ key] to advance to the start of the next
     interval. *)
        intervalNumStart t (Key.succ key)

    let nextAlarmFiresAt (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs option =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            None
        else

        let key = InternalElt.key (pool t) elt

        if key = t.MaxIntervalNum then
            None
        else
            Some (nextAlarmFiresAtInternal t key)

    let nextAlarmFiresAtThrowing (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            raiseNextAlarmFiresAtThrowingOfEmptyTimingWheel ()

        let key = InternalElt.key (pool t) elt

        if key = t.MaxIntervalNum then
            raiseNextAlarmFiresAtWithAllAlarmsInMaxInterval ()

        nextAlarmFiresAtInternal t key

    let computeMaxAllowedAlarmTime t : TimeNs =
        let maxAllowedKey = PriorityQueue.maxAllowedKey t.PriorityQueue in

        if maxAllowedKey >= t.MaxIntervalNum then
            maxTime
        else
            TimeNs.add
                (intervalNumStartUnchecked t maxAllowedKey)
                ((AlarmPrecision.toSpan t.Config.AlarmPrecision) - TimeNs.Span.nanosecond)

    let nowIntervalNum (t : TimingWheel<'a>) : Key =
        PriorityQueue.minAllowedKey t.PriorityQueue

    let minAllowedAlarmIntervalNum = nowIntervalNum
    let maxAllowedAlarmIntervalNum (t : TimingWheel<'a>) : IntervalNum = intervalNum t t.MaxAllowedAlarmTime

    let intervalStart (t : TimingWheel<'a>) (time : TimeNs) : TimeNs =
        intervalNumStartUnchecked t (intervalNum t time)

    let invariant (inv : 'a -> unit) (t : TimingWheel<ExternalEltValue<'a>>) : unit =
        Config.invariant t.Config

        do
            if t.Start < minTime then
                failwith "expected start >= minTime"

            if t.Start > maxTime then
                failwith "expected start <= maxTime"

        do
            if t.MaxIntervalNum <> intervalNum t maxTime then
                failwith "incorrect MaxIntervalNum"

            if t.MaxIntervalNum <> intervalNum t (intervalNumStart t t.MaxIntervalNum) then
                failwith "incorrect MaxIntervalNum"

        do
            if t.Now < t.Start then
                failwith "expected Now after start"

            if t.Now > maxTime then
                failwith "expected Now before maxTime"

            if intervalNum t t.Now <> PriorityQueue.minAllowedKey t.PriorityQueue then
                failwith "expected now to be min allowed key"

        if t.NowIntervalNumStart <> intervalNumStart t (nowIntervalNum t) then
            failwith "incorrect NowIntervalNumStart"

        if t.MaxAllowedAlarmTime <> computeMaxAllowedAlarmTime t then
            failwith "incorrect MaxAllowedAlarmTime"

        PriorityQueue.invariant inv t.PriorityQueue

        PriorityQueue.iter
            t.PriorityQueue
            (fun alarm ->
                if Alarm.intervalNum t alarm <> intervalNum t (Alarm.atTime t alarm) then
                    failwith "bad interval num"

                if intervalStart t (Alarm.atTime t alarm) < intervalStart t t.Now then
                    failwith "future alarm started before now"

                if Alarm.atTime t alarm <= t.Now - alarmPrecision t then
                    failwith "alarm started in the past"
            )

    let advanceClock
        (t : TimingWheel<ExternalEltValue<'a>>)
        (toTime : TimeNs)
        (handleFired : ExternalElt -> unit)
        : unit
        =
        if toTime > t.Now then
            t.Now <- toTime
            let key = intervalNumUnchecked t toTime
            t.NowIntervalNumStart <- intervalNumStartUnchecked t key

            match PriorityQueue.increaseMinAllowedKey t.PriorityQueue key handleFired with
            | PriorityQueue.IncreaseMinAllowedKeyResult.MaxAllowedKeyDidNotChange ->
#if DEBUG
                assert (t.MaxAllowedAlarmTime = computeMaxAllowedAlarmTime t)
#endif
                ()
            | PriorityQueue.IncreaseMinAllowedKeyResult.MaxAllowedKeyMaybeChanged ->
                t.MaxAllowedAlarmTime <- computeMaxAllowedAlarmTime t

    let advanceClockStopAtNextAlarm t toTime handleFired =
        let minElt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull minElt then
            advanceClock t toTime (fun _ -> failwith "no firing allowed")
        else
            let key = InternalElt.key (pool t) minElt
            // as an optimization, compare against [interval_num_start] to avoid the potentially costly
            // computation of [Internal_elt.min_alarm_time]
            if toTime < (intervalNumStart t key) then
                advanceClock t toTime (fun _ -> failwith "no firing allowed")
            else
                let toTime = min toTime (InternalElt.minAlarmTime (pool t) minElt key)
                advanceClock t toTime handleFired

    let create<'a> (config : Config) (start : TimeNs) : TimingWheel<ExternalEltValue<'a>> =
        if start < TimeNs.epoch then
            failwith $"TimingWheel.create got start {start} before the epoch"

        let queue =
            PriorityQueue.create (config.Capacity |> Option.defaultValue 1) (Some config.LevelBits)

        let t =
            {
                Config = config
                Start = start
                MaxIntervalNum = intervalNumInternal maxTime config.AlarmPrecision
                Now = TimeNs.minValueFor1usRounding // set by advanceClock below
                NowIntervalNumStart = TimeNs.minValueFor1usRounding // set by advanceClock below
                MaxAllowedAlarmTime = maxTime // set by advanceClock below
                PriorityQueue = queue
            }

        t.MaxAllowedAlarmTime <- computeMaxAllowedAlarmTime t
        advanceClock t start (fun _ -> failwith "no firing")
        t

    let addAtIntervalNum (t : TimingWheel<ExternalEltValue<'a>>) (at : Key) (value : 'a) : ExternalElt =
        let numStart = intervalNumStart t at

        PriorityQueue.internalAdd t.PriorityQueue at numStart value
        |> InternalElt.toExternal

    let raiseThatFarInTheFuture (t : TimingWheel<'a>) (at : TimeNs) : unit =
        failwith $"TimingWheel cannot schedule alarm that far in the future (max: {t.MaxAllowedAlarmTime}; got: {at})"

    let raiseBeforeStartOfCurrentInterval (t : TimingWheel<'a>) (at : TimeNs) : unit =
        failwith
            $"TimingWheel cannot schedule alarm for {at} before start of current interval ({t.NowIntervalNumStart})"

    let ensureCanScheduleAlarm t atTime =
        if atTime > t.MaxAllowedAlarmTime then
            raiseThatFarInTheFuture t atTime

        if atTime < t.NowIntervalNumStart then
            raiseBeforeStartOfCurrentInterval t atTime

    let add t atTime value =
        ensureCanScheduleAlarm t atTime
        InternalElt.toExternal (PriorityQueue.internalAdd t.PriorityQueue (intervalNumUnchecked t atTime) atTime value)

    let remove (t : TimingWheel<ExternalEltValue<'a>>) (alarm : ExternalElt) : unit =
        PriorityQueue.remove t.PriorityQueue alarm

    let clear (t : TimingWheel<ExternalEltValue<'a>>) : unit = PriorityQueue.clear t.PriorityQueue
    let mem (t : TimingWheel<'a>) (alarm : ExternalElt) : bool = PriorityQueue.mem t.PriorityQueue alarm

    let rescheduleGen
        (t : TimingWheel<ExternalEltValue<'a>>)
        (alarm : ExternalElt)
        (key : Key)
        (atTime : TimeNs)
        : unit
        =
        if not (mem t alarm) then
            failwith "Timing_wheel cannot reschedule alarm not in timing wheel"

        ensureCanScheduleAlarm t atTime
        PriorityQueue.change t.PriorityQueue alarm key atTime

    let reschedule (t : TimingWheel<ExternalEltValue<'a>>) (alarm : ExternalElt) (atTime : TimeNs) : unit =
        rescheduleGen t alarm (intervalNumUnchecked t atTime) atTime

    let rescheduleAtIntervalNum (t : TimingWheel<ExternalEltValue<'a>>) (alarm : ExternalElt) (atTime : Key) : unit =
        rescheduleGen t alarm atTime (intervalNumStart t atTime)

    let minAlarmIntervalNum (t : TimingWheel<ExternalEltValue<'a>>) : Key option =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            None
        else
            Some (InternalElt.key (pool t) elt)

    let minAlarmIntervalNumThrowing (t : TimingWheel<ExternalEltValue<'a>>) : Key =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            failwith "minAlarmIntervalNumThrowing of empty timing_wheel"
        else
            InternalElt.key (pool t) elt

    let maxAlarmTimeInList (t : TimingWheel<ExternalEltValue<'a>>) (elt : InternalElt) : TimeNs =
        let pool = pool t
        InternalElt.maxAlarmTime pool elt (InternalElt.key pool elt)

    let minAlarmTimeInList (t : TimingWheel<ExternalEltValue<'a>>) (elt : InternalElt) : TimeNs =
        let pool = pool t
        InternalElt.minAlarmTime pool elt (InternalElt.key pool elt)

    let maxAlarmTimeInMinInterval (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs option =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            None
        else
            Some (maxAlarmTimeInList t elt)

    let minAlarmTimeInMinInterval (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs option =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            None
        else
            Some (minAlarmTimeInList t elt)

    let maxAlarmTimeInMinIntervalThrowing (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            failwith "maxAlarmTimeInMinIntervalThrowing of empty timing wheel"

        maxAlarmTimeInList t elt

    let minAlarmTimeInMinIntervalThrowing (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            failwith "minAlarmTimeInMinIntervalExn of empty timing wheel"

        minAlarmTimeInList t elt

    let firePastAlarms (t : TimingWheel<ExternalEltValue<'a>>) (handleFired : ExternalElt -> unit) : unit =
        PriorityQueue.firePastAlarms t.PriorityQueue handleFired (nowIntervalNum t) t.Now
