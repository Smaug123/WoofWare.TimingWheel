# WoofWare.TimingWheel

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.TimingWheel.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.TimingWheel)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.TimingWheel/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.TimingWheel/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.TimingWheel)](./LICENCE.md)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="logos/dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="logos/light.svg">
  <img alt="Project logo: minimalistic face of a cartoon Shiba Inu. Its outline suggests a clock face, and its tongue is a large minute hand, while one eyebrow is a severe minute hand." src="logos/light.svg" width="300">
</picture>

This is the timing wheel from [janestreet/core_kernel](https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184/timing_wheel).

# Description

To quote the original documentation:

> A timing wheel is a specialized priority queue for a set of time-based alarms.

More completely, again quoted from the original documentation:

## A specialized priority queue for a set of time-based alarms.

A timing wheel is a data structure that maintains a clock with the current time and a
set of alarms scheduled to fire in the future. One can add and remove alarms, and
advance the clock to cause alarms to fire. There is nothing asynchronous about a
timing wheel. Alarms only fire in response to an `advanceClock` call.

When one `create`s a timing wheel, one supplies an initial time, `start`, and an
`alarmPrecision`. The timing wheel breaks all time from the epoch onwards into
half-open intervals of size `alarmPrecision`, with the bottom half of each interval
closed, and the top half open. Alarms in the same interval fire in the same call to
`advanceClock`, as soon as `now t` is greater than all the times in the interval.
When an alarm `a` fires on a timing wheel `t`, the implementation guarantees that:

```fsharp
Alarm.atTime a < now t
```

That is, alarms never fire early. Furthermore, the implementation guarantees that
alarms don't go off too late. More precisely, for all alarms `a` in `t`:

```fsharp
  intervalStart t (Alarm.atTime a) >= intervalStart t (now t)
```

This implies that for all alarms `a` in `t`:

```fsharp
  Alarm.atTime a > now t - alarmPrecision t
```

Of course, an `advanceClock` call can advance the clock to an arbitrary time in the
future, and thus alarms may fire at a clock time arbitrarily far beyond the time for
which they were set. But the implementation has no control over the times supplied to
`advanceClock`; it can only guarantee that alarms will fire when `advanceClock` is
called with a time at least `alarmPrecision` greater than their scheduled time.

### Implementation

A timing wheel is implemented using a specialized priority queue in which the
half-open intervals from the epoch onwards are numbered 0, 1, 2, etc. Each time is
stored in the priority queue with the key of its interval number. Thus all alarms with
a time in the same interval get the same key, and hence fire at the same time. More
specifically, an alarm is fired when the clock reaches or passes the time at the start
of the next interval.

Alarms that fire in the same interval will fire in the order in which they were added
to the timing wheel, rather than the time they were set to go off. This is consistent
with the guarantees of timing wheel mentioned above, but may nonetheless be surprising
to users.

The priority queue is implemented with an array of levels of decreasing precision,
with the lowest level having the most precision and storing the closest upcoming
alarms, while the highest level has the least precision and stores the alarms farthest
in the future. As time increases, the timing wheel does a lazy radix sort of the alarm
keys.

This implementation makes `addAlarm] and `removeAlarm` constant time, while
`advanceClock` takes time proportional to the amount of time the clock is advanced.
With a sufficient number of alarms, this is more efficient than a log(N) heap
implementation of a priority queue.

### Representable times

A timing wheel `t` can only handle a (typically large) bounded range of times as
determined by the current time, `now t`, and the `levelBits` and `alarmPrecision`
arguments supplied to `create`. Various functions raise if they are supplied a time
before the start of the current interval (i.e. `now t` rounded down to `alarmPrecision`)
or `> maxAllowedAlarmTime t`. This situation likely indicates
a misconfiguration of the `levelBits` and/or `alarmPrecision`. Here is the duration
of `maxAllowedAlarmTime t - now t` using the default `levelBits`.

```
  | # intervals | alarm_precision | duration |
  +-------------+-----------------+----------|
  |        2^61 | nanosecond      | 73 years |
```

# Status

This was my first pass at porting the library.

I have ported all its tests, but I have *not* used the library in prod, so I have very little guarantee that the
semantics of WoofWare.TimingWheel are even remotely similar to those of `timing_wheel`.
I draw your attention specifically to these words from the MIT licence:

> WITHOUT WARRANTY OF ANY KIND... INCLUDING... FITNESS FOR A PARTICULAR PURPOSE

# To-do list

* Tighten the API surface so that specifically only the useful methods are exposed.
* Perhaps pull out the `tuple_pool` implementation which I have inlined; OCaml is operating under very different constraints to F# for that.
* Document the API surface.

# Licence

This is a derivative work of [core_kernel](https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184), used under the MIT licence.
A copy of that licence is at [LICENCE_janestreet.md](LICENCE_janestreet.md).
All glory to Jane Street.

WoofWare.TimingWheel is licenced to you under the MIT licence.
A copy of that licence is at [LICENCE.md](LICENCE.md).
