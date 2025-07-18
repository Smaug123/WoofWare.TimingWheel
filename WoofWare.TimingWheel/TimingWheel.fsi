namespace WoofWare.TimingWheel

type TimingWheel<'a> =
    internal
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
    type Alarm = Elt

    [<RequireQualifiedAccess>]
    module Alarm =
        /// <summary>
        /// An alarm which is not in any timing wheel.
        /// </summary>
        val null' : Alarm

        /// <summary>
        /// Look up the time at which this alarm will fire.
        /// </summary>
        /// <exception cref="Exception">Throws if <c>not (TimingWheel.mem t alarm)</c>.</exception>
        val atTime<'a> : ExternalEltValue<'a> TimingWheel -> Alarm -> TimeNs

        /// <summary>
        /// Look up the interval number of the time at which this alarm will fire.
        /// </summary>
        /// <exception cref="Exception">Throws if <c>not (TimingWheel.mem t alarm)</c>.</exception>
        val intervalNum<'a> : ExternalEltValue<'a> TimingWheel -> Alarm -> IntervalNum

        /// <summary>
        /// The value that will be supplied to the alarm-firing handler when this alarm is fired.
        /// </summary>
        /// <exception cref="Exception">Throws if <c>not (TimingWheel.mem t alarm)</c>.</exception>
        val value<'a> : ExternalEltValue<'a> TimingWheel -> Alarm -> 'a

    /// <summary>
    /// <c>create config start</c> creates a new timing wheel with current time <c>start</c>.
    /// </summary>
    /// <remarks>
    /// For a fixed <c>levelBits</c>, a smaller (i.e. more precise) <c>alarmPrecision</c> decreases the representable
    /// range of times/keys and increases the constant factor for <c>advanceClock</c>.
    /// </remarks>
    /// <param name="config"></param>
    /// <param name="start">current time of the wheel</param>
    /// <exception cref="InvalidArgumentException"><c>start &lt; TimeNs.epoch</c></exception>
    val create<'a> : config : Config -> start : TimeNs -> 'a ExternalEltValue TimingWheel

    /// <summary>
    /// Accessor: the length of the equally-sized intervals this wheel divides time into.
    /// </summary>
    val alarmPrecision<'a> : TimingWheel<'a> -> TimeNs.Span

    /// <summary>
    /// Accessor: the timing wheel's current time.
    /// </summary>
    /// <remarks>
    /// For example, you can't register alarms for before this time, because that's in the past.
    /// </remarks>
    val now<'a> : 'a TimingWheel -> TimeNs

    /// <summary>
    /// Accessor: the time that it was when the TimingWheel was first created.
    /// </summary>
    val start<'a> : 'a TimingWheel -> TimeNs

    /// <summary>
    /// Whether any alarms are currently waiting in this TimingWheel.
    /// </summary>
    /// <remarks>
    /// Alarms which were present but have fired are considered no longer present; so a timing wheel in which all
    /// alarms have fired is considered "empty".
    /// </remarks>
    val isEmpty<'a> : 'a TimingWheel -> bool

    /// <summary>
    /// Total number of (not-yet-fired) alarms stored in the TimingWheel.
    /// </summary>
    val length<'a> : 'a TimingWheel -> int

    /// <summary>
    /// Do something for every (not-yet-fired) alarm in the TimingWheel.
    /// </summary>
    val iter<'a> : 'a ExternalEltValue TimingWheel -> (Alarm -> unit) -> unit

    /// <summary>
    /// The number of the interval that <c>time</c> is in.
    /// </summary>
    /// <remarks>
    /// Interval <c>0</c> is the interval that starts at <c>TimeNs.epoch</c>.
    /// </remarks>
    /// <exception cref="InvalidArgumentException">The input time is less than <c>TimeNs.epoch</c>.</exception>
    val intervalNum<'a> : 'a TimingWheel -> time : TimeNs -> IntervalNum

    /// <summary>
    /// The interval num containing the timing wheel's current time.
    /// </summary>
    /// <remarks>
    /// This is by definition <c>intervalNum t (now t)</c>.
    /// </remarks>
    val nowIntervalNum<'a> : 'a TimingWheel -> IntervalNum

    /// <summary>
    /// The start of the <c>intervalNum</c>'th interval in the wheel.
    /// That is, <c>intervalNum * alarmPrecision t</c> after the epoch.
    /// </summary>
    val intervalNumStart<'a> : 'a TimingWheel -> intervalNum : IntervalNum -> TimeNs

    /// <summary>
    /// The start of the half-open interval containing <c>time</c>.
    /// </summary>
    /// <remarks>
    /// This is by definition <c>intervalNumStart t (intervalNum t time)</c>.
    /// </remarks>
    val intervalStart<'a> : 'a TimingWheel -> time : TimeNs -> TimeNs

    /// <summary>
    /// Advances the clock to the given time, firing and removing all alarms which would trigger strictly before the
    /// start of the interval containing that time.
    /// </summary>
    /// <remarks>
    /// If <c>toTime &lt;= now t</c>, this call does nothing.
    ///
    /// Succeeds if <c>toTime</c> is too far in the future to represent, but subsequent attempts to <c>add</c> will fail in that case.
    ///
    /// Behaviour is unspecified if the callback accesses the timing wheel in any way other than through <c>Alarm</c> functions.
    /// </remarks>
    val advanceClock<'a> : 'a ExternalEltValue TimingWheel -> toTime : TimeNs -> (Alarm -> unit) -> unit

    /// <summary>
    /// Advance to the time <c>toTime</c> or the time of the next alarm, whichever is earlier.
    /// </summary>
    /// <remarks>
    /// This is functionally equivalent to <c>advanceClock t (Time.min toTime (minAlarmTimeInMinInterval t))</c>,
    /// but has potentially better performance.
    ///
    /// The callback may still fire multiple times, if there are multiple alarms scheduled for the same time.
    /// TODO: why can this ever fire at all? surely we've advanced to just before the alarm?
    /// </remarks>
    val advanceClockStopAtNextAlarm<'a> : 'a ExternalEltValue TimingWheel -> toTime : TimeNs -> (Alarm -> unit) -> unit

    /// <summary>
    /// Fires and removes all alarms in the timing wheel with <c>Alarm.at t a &lt;= now t</c>, triggering the
    /// callback for each such alarm.
    /// </summary>
    /// <remarks>
    /// Visits all alarms in the current interval, to check their triggering time.
    ///
    /// Behaviour is unspecified if the callback accesses the timing wheel in any way other than through <c>Alarm</c>
    /// functions.
    ///
    /// TODO: is it the case that the only reason this can fire an alarm is if the current time is *precisely* an
    /// alarm time?
    /// </remarks>
    val firePastAlarms : 'a ExternalEltValue TimingWheel -> (Alarm -> unit) -> unit

    /// <summary>
    /// The greatest time that can be supplied to <c>add</c>.
    /// </summary>
    /// <remarks>
    /// This is not constant; its value increases as <c>now</c> increases.
    /// </remarks>
    val maxAllowedAlarmTime : 'a TimingWheel -> TimeNs

    /// <summary>
    /// The smallest interval num that can be supplied to <c>addAtIntervalNum</c>.
    /// </summary>
    /// <remarks>
    /// This is by definition <c>nowIntervalNum</c>.
    /// </remarks>
    val minAllowedAlarmIntervalNum : 'a TimingWheel -> IntervalNum

    /// <summary>
    /// The largest interval num that can be supplied to <c>addAtIntervalNum</c>.
    /// </summary>
    /// <remarks>
    /// This is by definition <c>intervalNum t (maxAllowedAlarmTime t)</c>.
    /// </remarks>
    val maxAllowedAlarmIntervalNum : 'a TimingWheel -> IntervalNum

    /// <summary>
    /// Adds a new value to the timing wheel, returning an <c>Alarm</c> that will fire at the specified time.
    /// </summary>
    /// <remarks>
    /// The returned <c>Alarm</c> can be supplied to <c>remove</c> to unregister the alarm.
    /// </remarks>
    /// <param name="atTime"></param>
    /// <exception cref="Exception">Throws if <c>intervalNum t atTime &lt; nowIntervalNum t</c> or <c>atTime > maxAllowedAlarmTime t</c>.</exception>
    val add<'a> : 'a ExternalEltValue TimingWheel -> atTime : TimeNs -> 'a -> Alarm

    /// <summary>
    /// Adds a new value to the timing wheel, returning an <c>Alarm</c> that will fire at the start of the specified
    /// interval.
    /// </summary>
    /// <remarks>
    /// The returned <c>Alarm</c> can be supplied to <c>remove</c> to unregister the alarm.
    /// </remarks>
    val addAtIntervalNum<'a> : 'a ExternalEltValue TimingWheel -> atTime : IntervalNum -> 'a -> Alarm

    /// <summary>
    /// True if the supplied alarm is present and not-yet-fired in the wheel.
    /// </summary>
    val mem<'a> : 'a TimingWheel -> Alarm -> bool

    /// <summary>
    /// Removes the specified alarm from the timing wheel, so it won't fire when the wheel reaches its time.
    /// </summary>
    /// <exception cref="Exception">Throws if <c>not (mem t alarm)</c>.</exception>
    val remove<'a> : 'a ExternalEltValue TimingWheel -> Alarm -> unit

    /// <summary>
    /// Mutates the alarm so that it fires at the new specified time instead.
    /// </summary>
    /// <exception cref="Exception">Raises if <c>not (mem t alarm)</c>, or if <c>atTime</c> is an invalid time for the wheel (in the sense of the <c>add</c> function).</exception>
    val reschedule<'a> : ExternalEltValue<'a> TimingWheel -> Alarm -> atTime : TimeNs -> unit

    /// <summary>
    /// Mutates the alarm so that it fires at the start of the given interval instead.
    /// </summary>
    /// <remarks>
    /// Equivalent to <c>reschedule t alarm (intervalNumStart t at)</c>.
    /// </remarks>
    val rescheduleAtIntervalNum<'a> : ExternalEltValue<'a> TimingWheel -> Alarm -> at : IntervalNum -> unit

    /// <summary>
    /// Removes all alarms from the wheel.
    /// </summary>
    val clear<'a> : 'a ExternalEltValue TimingWheel -> unit

    /// <summary>
    /// The smallest <c>Alarm.intervalNum</c> of all alarms in the wheel.
    /// </summary>
    val minAlarmIntervalNum<'a> : 'a ExternalEltValue TimingWheel -> IntervalNum option

    /// <summary>
    /// The smallest <c>Alarm.intervalNum</c> of all alarms in the wheel.
    /// </summary>
    /// <exception cref="Exception">Raises if there are no alarms in the wheel.</exception>
    val minAlarmIntervalNumThrowing<'a> : 'a ExternalEltValue TimingWheel -> IntervalNum

    /// <summary>
    /// The maximum alarm time over all alarms in the earliest interval of the wheel.
    /// </summary>
    /// <remarks>
    /// This is useful for advancing to the <c>minAlarmIntervalNum</c> of a timing wheel and then
    /// calling <c>firePastAlarms</c> to fire the alarms in that interval.
    /// This is useful when simulating time, to ensure that alarms are processed in order.
    /// </remarks>
    val maxAlarmTimeInMinInterval<'a> : 'a ExternalEltValue TimingWheel -> TimeNs option

    /// <summary>
    /// The minimum alarm time over all alarms in the wheel.
    /// </summary>
    /// <remarks>
    /// This is useful for advancing to the exact time when the next alarm is scheduled to fire.
    /// </remarks>
    val minAlarmTimeInMinInterval<'a> : 'a ExternalEltValue TimingWheel -> TimeNs option

    /// <summary>
    /// The maximum alarm time over all alarms in the earliest interval of the wheel.
    /// </summary>
    /// <remarks>
    /// This is <c>maxAlarmTimeInMinInterval</c>, but throws if there are no alarms in the wheel.
    /// </remarks>
    /// <exception cref="Exception">Throws if there are no alarms in the wheel.</exception>
    val maxAlarmTimeInMinIntervalThrowing<'a> : 'a ExternalEltValue TimingWheel -> TimeNs

    /// <summary>
    /// The minimum alarm time over all alarms in the wheel.
    /// </summary>
    /// <remarks>
    /// This is <c>minAlarmTimeInMinInterval</c>, but throws if there are no alarms in the wheel.
    /// </remarks>
    /// <exception cref="Exception">Throws if there are no alarms in the wheel.</exception>
    val minAlarmTimeInMinIntervalThrowing<'a> : 'a ExternalEltValue TimingWheel -> TimeNs

    (** The name of this function is misleading: it does not take into account events that
      can fire due to [fire_past_alarms].

      [next_alarm_fires_at t] returns the minimum time to which the clock can be advanced
      such that an alarm will be fired by [advance_clock], or [None] if [t] has no alarms
      (or all alarms are in the max interval, and hence cannot fire by [advance_clock]).
      If [next_alarm_fires_at t = Some next], then for the minimum alarm time [min] that
      occurs in [t], it is guaranteed that: [next - alarm_precision t <= min < next]. *)
    val nextAlarmFiresAt<'a> : 'a ExternalEltValue TimingWheel -> TimeNs option

    /// <summary>
    ///
    /// </summary>
    /// <remarks>
    /// This is <c></c>, but throws if there are no alarms in the wheel.
    /// </remarks>
    /// <exception cref="Exception">Throws if there are no alarms in the wheel.</exception>
    val nextAlarmFiresAtThrowing<'a> : 'a ExternalEltValue TimingWheel -> TimeNs

    val internal maxTime : TimeNs
    val internal maxIntervalNum<'a> : TimingWheel<'a> -> IntervalNum
    val internal invariant<'a> : ('a -> unit) -> ExternalEltValue<'a> TimingWheel -> unit
    val internal intervalNumInternal : TimeNs -> AlarmPrecision -> IntervalNum
