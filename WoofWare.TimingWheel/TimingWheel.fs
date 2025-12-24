namespace WoofWare.TimingWheel

type TimingWheel<'a> =
    internal
        {
            Config : TimingWheelConfig
            Start : TimeNs
            MaxIntervalNum : IntervalNum
            mutable Now : TimeNs
            mutable NowIntervalNumStart : TimeNs
            mutable MaxAllowedAlarmTime : TimeNs
            PriorityQueue : PriorityQueue<'a>
        }

[<RequireQualifiedAccess>]
module TimingWheel =

    let internal config<'a> (t : TimingWheel<'a>) : TimingWheelConfig = t.Config

    let alarmPrecision<'a> (t : TimingWheel<'a>) : TimeNs.Span =
        TimingWheelConfig.alarmPrecision t.Config

    /// The earliest time that ever existed for this TimingWheel.
    let start<'a> (t : TimingWheel<'a>) : TimeNs = t.Start

    /// The current time, according to this TimingWheel. For example, you can't register alarms for before this time,
    /// because that's in the past.
    let now<'a> (t : TimingWheel<'a>) : TimeNs = t.Now

    let maxIntervalNum<'a> (t : TimingWheel<'a>) : IntervalNum = t.MaxIntervalNum

    /// The latest time you can set an alarm for. (We don't have enough precision to register alarms arbitrarily far
    /// in the future, for example.)
    let maxAllowedAlarmTime<'a> (t : TimingWheel<'a>) : TimeNs = t.MaxAllowedAlarmTime

    type Alarm = Elt

    [<RequireQualifiedAccess>]
    module Alarm =
        let null' : Alarm = Elt.null'
        let atTime<'a> (tw : TimingWheel<ExternalEltValue<'a>>) (t : ExternalElt) : TimeNs = Elt.at tw.PriorityQueue t
        let value<'a> (tw : TimingWheel<ExternalEltValue<'a>>) (t : ExternalElt) : 'a = Elt.value tw.PriorityQueue t

        let intervalNum<'a> (tw : TimingWheel<ExternalEltValue<'a>>) (t : ExternalElt) : Key =
            Elt.key tw.PriorityQueue t

    let iter<'a> (t : TimingWheel<ExternalEltValue<'a>>) (f : Elt -> unit) : unit = PriorityQueue.iter t.PriorityQueue f

    let length<'a> (t : TimingWheel<'a>) : int = t.PriorityQueue.Length
    let isEmpty<'a> (t : TimingWheel<'a>) : bool = length t = 0

    let private raiseNextAlarmFiresAtThrowingOfEmptyTimingWheel () =
        failwith "nextAlarmFiresAtThrowing of empty timing wheel"

    let private raiseNextAlarmFiresAtWithAllAlarmsInMaxInterval () =
        failwith "nextAlarmFiresAtThrowing with all alarms in max interval"

    let internal pool<'a> (t : TimingWheel<'a>) : Pool<'a> = t.PriorityQueue.Pool

    let intervalNumInternal (time : TimeNs) (alarmPrecision : AlarmPrecision) : IntervalNum =
        IntervalNum.ofInt64 (AlarmPrecision.intervalNum alarmPrecision time)

    let intervalNumUnchecked<'a> (t : TimingWheel<'a>) (time : TimeNs) : IntervalNum =
        intervalNumInternal time t.Config.AlarmPrecision

    let minTime : TimeNs = TimeNs.epoch
    let maxTime : TimeNs = TimeNs.maxValueRepresentable
    let minIntervalNum : IntervalNum = IntervalNum.zero

    let intervalNum<'a> (t : TimingWheel<'a>) (time : TimeNs) : IntervalNum =
        if time < minTime then
            invalidArg "time" $"intervalNum got time too far in the past: {time}, min is {minTime}"

        intervalNumUnchecked t time

    let intervalNumStartUnchecked<'a> (t : TimingWheel<'a>) (intervalNum : IntervalNum) : TimeNs =
        AlarmPrecision.intervalNumStart t.Config.AlarmPrecision (intervalNum |> IntervalNum.toInt64)

    let private raiseIntervalNumStartGotTooSmall (intervalNum : IntervalNum) : unit =
        failwith $"intervalNumStart got too small intervalNum: {intervalNum}, min was {minIntervalNum}"

    let private raiseIntervalNumStartGotTooLarge (t : TimingWheel<'a>) (intervalNum : IntervalNum) : unit =
        failwith $"intervalNumStart got too large intervalNum: {intervalNum}, max was {t.MaxIntervalNum}"

    let intervalNumStart<'a> (t : TimingWheel<'a>) (intervalNum : IntervalNum) : TimeNs =
        if intervalNum < minIntervalNum then
            raiseIntervalNumStartGotTooSmall intervalNum

        if intervalNum > t.MaxIntervalNum then
            raiseIntervalNumStartGotTooLarge t intervalNum

        intervalNumStartUnchecked t intervalNum

    let nextAlarmFiresAtInternal<'a> (t : TimingWheel<'a>) (key : Key) : TimeNs =
        (* [interval_num_start t key] is the key corresponding to the start of the time interval
     holding the first alarm in [t].  Advancing to that would not be enough, since the
     alarms in that interval don't fire until the clock is advanced to the start of the
     next interval.  So, we use [succ key] to advance to the start of the next
     interval. *)
        intervalNumStart t (Key.succ key)

    let nextAlarmFiresAt<'a> (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs option =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            None
        else

        let key = InternalElt.key (pool t) elt

        if key = t.MaxIntervalNum then
            None
        else
            Some (nextAlarmFiresAtInternal t key)

    let nextAlarmFiresAtThrowing<'a> (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            raiseNextAlarmFiresAtThrowingOfEmptyTimingWheel ()

        let key = InternalElt.key (pool t) elt

        if key = t.MaxIntervalNum then
            raiseNextAlarmFiresAtWithAllAlarmsInMaxInterval ()

        nextAlarmFiresAtInternal t key

    let computeMaxAllowedAlarmTime<'a> (t : TimingWheel<'a>) : TimeNs =
        let maxAllowedKey = PriorityQueue.maxAllowedKey t.PriorityQueue in

        if maxAllowedKey >= t.MaxIntervalNum then
            maxTime
        else
            TimeNs.add
                (intervalNumStartUnchecked t maxAllowedKey)
                ((AlarmPrecision.toSpan t.Config.AlarmPrecision) - TimeNs.Span.nanosecond)

    let nowIntervalNum<'a> (t : TimingWheel<'a>) : Key =
        PriorityQueue.minAllowedKey t.PriorityQueue

    let minAllowedAlarmIntervalNum<'a> (t : TimingWheel<'a>) : Key = nowIntervalNum t
    let maxAllowedAlarmIntervalNum<'a> (t : TimingWheel<'a>) : IntervalNum = intervalNum t t.MaxAllowedAlarmTime

    let intervalStart<'a> (t : TimingWheel<'a>) (time : TimeNs) : TimeNs =
        intervalNumStartUnchecked t (intervalNum t time)

    let invariant<'a> (inv : 'a -> unit) (t : TimingWheel<ExternalEltValue<'a>>) : unit =
        TimingWheelConfig.invariant t.Config

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

                if Alarm.atTime t alarm <= TimeNs.sub t.Now (alarmPrecision t) then
                    failwith "alarm started in the past"
            )

    let advanceClock<'a>
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

    let advanceClockStopAtNextAlarm<'a>
        (t : TimingWheel<ExternalEltValue<'a>>)
        (toTime : TimeNs)
        (handleFired : ExternalElt -> unit)
        : unit
        =
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

    let create<'a> (config : TimingWheelConfig) (start : TimeNs) : TimingWheel<ExternalEltValue<'a>> =
        if start < TimeNs.epoch then
            invalidArg "start" $"TimingWheel.create got start {start} before the epoch"

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

    let addAtIntervalNum<'a> (t : TimingWheel<ExternalEltValue<'a>>) (atTime : Key) (value : 'a) : ExternalElt =
        let numStart = intervalNumStart t atTime

        PriorityQueue.internalAdd t.PriorityQueue atTime numStart value
        |> InternalElt.toExternal

    let private raiseThatFarInTheFuture (t : TimingWheel<'a>) (at : TimeNs) : unit =
        failwith $"TimingWheel cannot schedule alarm that far in the future (max: {t.MaxAllowedAlarmTime}; got: {at})"

    let private raiseBeforeStartOfCurrentInterval (t : TimingWheel<'a>) (at : TimeNs) : unit =
        failwith
            $"TimingWheel cannot schedule alarm for {at} before start of current interval ({t.NowIntervalNumStart})"

    let ensureCanScheduleAlarm<'a> (t : TimingWheel<'a>) (atTime : TimeNs) : unit =
        if atTime > t.MaxAllowedAlarmTime then
            raiseThatFarInTheFuture t atTime

        if atTime < t.NowIntervalNumStart then
            raiseBeforeStartOfCurrentInterval t atTime

    let add<'a> (t : TimingWheel<ExternalEltValue<'a>>) (atTime : TimeNs) (value : 'a) : ExternalElt =
        ensureCanScheduleAlarm t atTime
        InternalElt.toExternal (PriorityQueue.internalAdd t.PriorityQueue (intervalNumUnchecked t atTime) atTime value)

    let remove<'a> (t : TimingWheel<ExternalEltValue<'a>>) (alarm : ExternalElt) : unit =
        PriorityQueue.remove t.PriorityQueue alarm

    let clear<'a> (t : TimingWheel<ExternalEltValue<'a>>) : unit = PriorityQueue.clear t.PriorityQueue
    let mem<'a> (t : TimingWheel<'a>) (alarm : ExternalElt) : bool = PriorityQueue.mem t.PriorityQueue alarm

    let rescheduleGen<'a>
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

    let reschedule<'a> (t : TimingWheel<ExternalEltValue<'a>>) (alarm : ExternalElt) (atTime : TimeNs) : unit =
        rescheduleGen t alarm (intervalNumUnchecked t atTime) atTime

    let rescheduleAtIntervalNum<'a> (t : TimingWheel<ExternalEltValue<'a>>) (alarm : ExternalElt) (at : Key) : unit =
        rescheduleGen t alarm at (intervalNumStart t at)

    let minAlarmIntervalNum<'a> (t : TimingWheel<ExternalEltValue<'a>>) : Key option =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            None
        else
            Some (InternalElt.key (pool t) elt)

    let minAlarmIntervalNumThrowing<'a> (t : TimingWheel<ExternalEltValue<'a>>) : Key =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            failwith "minAlarmIntervalNumThrowing of empty timing_wheel"
        else
            InternalElt.key (pool t) elt

    let maxAlarmTimeInList<'a> (t : TimingWheel<ExternalEltValue<'a>>) (elt : InternalElt) : TimeNs =
        let pool = pool t
        InternalElt.maxAlarmTime pool elt (InternalElt.key pool elt)

    let minAlarmTimeInList<'a> (t : TimingWheel<ExternalEltValue<'a>>) (elt : InternalElt) : TimeNs =
        let pool = pool t
        InternalElt.minAlarmTime pool elt (InternalElt.key pool elt)

    let maxAlarmTimeInMinInterval<'a> (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs option =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            None
        else
            Some (maxAlarmTimeInList t elt)

    let minAlarmTimeInMinInterval<'a> (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs option =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            None
        else
            Some (minAlarmTimeInList t elt)

    let maxAlarmTimeInMinIntervalThrowing<'a> (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs =
        let elt = PriorityQueue.minElt' t.PriorityQueue in

        if InternalElt.isNull elt then
            failwith "maxAlarmTimeInMinIntervalThrowing of empty timing wheel"

        maxAlarmTimeInList t elt

    let minAlarmTimeInMinIntervalThrowing<'a> (t : TimingWheel<ExternalEltValue<'a>>) : TimeNs =
        let elt = PriorityQueue.minElt' t.PriorityQueue

        if InternalElt.isNull elt then
            failwith "minAlarmTimeInMinIntervalThrowing of empty timing wheel"

        minAlarmTimeInList t elt

    let firePastAlarms<'a> (t : TimingWheel<ExternalEltValue<'a>>) (handleFired : ExternalElt -> unit) : unit =
        PriorityQueue.firePastAlarms t.PriorityQueue handleFired (nowIntervalNum t) t.Now
