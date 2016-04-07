# CSP_E: Log analyzing tool for concurrent systems
Copyright (C) 2014, 2015, 2016: National Institute of Advanced Science and Technology (AIST)

CSP_E is a log analyzing tool which is based on Hoare's CSP notation.  CSP_E as a library provides a shallow-embedded language on Scala, also called CSP_E.  A event monitor can be defined by using CSP_E language.  A log analyzer then parses a log, and feeds an event stream into a monitor.  If a monitor fails, it indicates that something unexpected happened.

CSP_E does not provides parsing facilities of a log.  Instead, CSP_E monitor (Process) accepts Event class as an element of an input stream.

## Classes and methods

Everything is packed under jp.go.aist.cspe module.

### Event

Event class is a case class which has a symbol as a first parameter, and arbitrary number of arguments of Any type.  Event class has the base class AbsEvent.  A monitor which is defined by CSP_E, accepts AbsEvent and creates a new monitor which contains the next state.

### ProcessSet

ProcessSet class represents a set of monitors.  ProcessSet implements the methods which accepts events or event streams.

Method | Action |
-------|--------|
`ps << e` | Accepts event `e` and returns the next states.  `ps` does not change its internal state.
`ps canTerminate` | Returns whether `ps` contains a monitor which can be terminated immediately, as Boolean
`ps |= es`  | Accepts `es: Traversable[AbsEveT]` and returns whether `ps` can accept `es` or not as Boolean
`ps isFailure` | Returns `true` if `ps` does not contain a valid state, and `false` if `ps` contains a valid state.

### SKIP

This singleton object represents a process which does nothing and terminates immediately.

### STOP

This singleton object represents a deadlock.

### Failure

Failure is a new construct to adapt CSP for event monitoring.  Failure represents a process which is already failed to accept a event.  Failure is used to show that the state which leads to Failure should not happens.

### CSPE

This is the Builder of monitors and ProcessSet.  CSPE is supposed to be used by importing all public methods of this singleton object.

Method | Action
-------|-------
`? g p` | Guard
`?? {case Event('...) ...}` | If it receive mathing event, execute the matching clause.  Otherwise, it fails.
`??? {case Event('...) ...}` | If it receive mathing event, execute the matching clause.  Otherwise, it just waits.
`choice(Set(p1, p2, ...))` | External choice
`parallel(Bag(p1, p2, ...), Set('a, 'b, ...))` | Parallel composition of `p1, p2,...` using `'a, 'b...` as synchronization events
`sequence(p1 : p2 : ...)` | Sequential composition of `p1, p2, ...`
`interrupt(p, Set('a, 'b,...), q)` | Interrupt process `p` when one of `'a, ...'` occurs and execute process `q`
`processSet(Set(p1, p2, ...))` | Builder method for `ProcessSet`

### Process

Process is the class for an individual monitor.

Method | Action
-------|--------
`e -> p` | Prefix
`p1 <+> p2` | External choice
`p1 || a p2` | Parallel of `p1` and `p2` using set `a` of alphabets as synchronization events
`p1 ||| p2` | Interleaving
`p1 $ p2` | Sequential composition
`p | a q` | Interrupt using `a` as interrupt events

## Example

The monitor which tests the open and close events are matched.
```scala
def openCloseSimpl : Process = ?? {
  case Event('open) =>
     (openCloseSimpl <+> SKIP) ||| ?? { case Event('close) => SKIP}
  case Event('close) => Failure
  case _ => openCloseSimpl <+> SKIP
}
```

The monitor which check the auction bidding.

```scala
def auction(max : Int) : Process = ?? {
      case Event('bid, p : Int) => ? (p > max) {auction(p)}
    }
```

A monitor for Unix like processes and file descriptros.
```scala
def childProcess(pid : Int, openFiles : Set[Int]) : Process =
  run(pid, openFiles) $ Event('Exit, `pid`) ->: SKIP

def run(pid : Int, openFiles : Set[Int]) : Process = ?? {
  case Event('Access, `pid`, fd : Int) if openFiles(fd) =>
    run(pid, openFiles)
  case Event('Open, `pid`, fd : Int) if !openFiles(fd) =>
    run(pid, openFiles + fd) $ Event('Close, pid, fd) ->: run(pid, openFiles)
  case Event('Spawn, `pid`, child_pid : Int) =>
    run(pid, openFiles) ||| childProcess(child_pid, openFiles)
} <+> SKIP

var monitors = new ProcessSet(List(run(0, Set.empty)))
```

The last monitor processes 10K events about 6 seconds on Mac Pro.

## Semantics

### Informal Semantics

- `e -> p` : The process which accepts the event `e`, then behaves like `p`
- `p1 <+> p2` : The process which behaves either like `p1` or `p2`, depending on the event that it accepts.  If both of `p1` and `p2` can accept the event, both possibilities remain.
- `p1 || a p2` : Parallel composition of `p1` and `p2` using events of which alphabets are elements of `a` as synchronization events
- `p1 ||| p2` : interleaving of `p1` and `p2`
- `p1 $ p2` : The process which first behaves like `p1`, then behaves like `p2`
- `p | a q` : The process which behaves like `p`, but if an event of which alphabet is in `a` occurs, behaves like `q`

### Formal Semantics

See the forthcoming paper

## Gotcha

The small change of model can cause the large difference of performance.  For example,
```scala
val openCloseExplode =
  process (p =>
    ?? {
      case Event('open) =>
        (p <+> SKIP) ||| ?? { case Event('close) => SKIP}
//    case Event('close) => Failure
      case _ => p <+> SKIP
    }
  )
```
which comments out only one line from the above example, leads the state explosion.
