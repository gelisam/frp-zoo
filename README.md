![A window with 6 buttons labelled "0", "5", "10", "toggle", "toggle", and "toggle".](toy-app-thumbnail.png)

# FRP Zoo

Interested in trying FRP (Functional Reactive Programming), but overwhelmed by the number of FRP libraries to choose from? To help you with this choice, this repository contains several implementations of the same small program, to give you a taste of what each library looks like.

library | classification | example app
:---:|:---:|:---:
[artery](https://hackage.haskell.org/package/artery) | scenarios 0 and 5, arrowized, signals | [code](artery-example/Main.hs)
[auto](https://hackage.haskell.org/package/auto) | scenarios 0 and 10, arrowized, signals | [code](auto-example/Main.hs)
[DysFRP](https://hackage.haskell.org/package/DysFRP) | scenarios 0 and 10, higher-order, behaviours and events | [code](DysFRP/Main.hs)
[elerea](https://hackage.haskell.org/package/elerea) | scenarios 0 and 5, higher-order, signals | [code](elerea-example/Main.hs)
[euphoria](https://hackage.haskell.org/package/euphoria) | scenario 0, higher-order, step signals, behaviours and events | [code](euphoria-example/Main.hs)
[FRPNow](https://hackage.haskell.org/package/frpnow) | scenario 0, higher-order, behaviours and events | [code](frpnow-example/Main.hs)
[grapefruit](https://hackage.haskell.org/package/grapefruit-frp) | scenario 0, higher-order, step signals, behaviours and events | [code](grapefruit-example/Main.hs)
[machinecell](https://hackage.haskell.org/package/machinecell) | scenarios 0 and 5, arrowized, signals | [code](machinecell-example/Main.hs)
[netwire](https://hackage.haskell.org/package/netwire) | all three scenarios, arrowized, continuous, signals | [code](netwire-example/Main.hs)
[varying](https://hackage.haskell.org/package/varying) | all three scenarios, arrowized or applicative, continuous, signals | [code](varying-example/Main.hs)
[ordrea](https://hackage.haskell.org/package/ordrea) | scenario 0, higher-order, step signals, behaviours and events | [code](ordrea-example/Main.hs)
[reactive-bacon](https://hackage.haskell.org/package/reactive-bacon) | scenarios 0 and 5, asynchronous data flow, behaviours and events | [code](reactive-bacon-example/Main.hs)
[reactive-banana](https://hackage.haskell.org/package/reactive-banana) | scenario 0, higher-order, behaviours and events | [code](reactive-banana-example/Main.hs)
[reflex](https://hackage.haskell.org/package/reflex) | scenarios 0 and 5, higher-order, behaviours and events | [code](reflex-example/Main.hs)
[sodium](https://hackage.haskell.org/package/sodium) | scenarios 0 and 5, higher-order, behaviours and events | [code](sodium-example/Main.hs)
[Yampa](https://hackage.haskell.org/package/Yampa) | scenarios 0 and 5, arrowized, continuous, signals | [code](Yampa-example/Main.hs)
[reactive](https://hackage.haskell.org/package/reactive) |  Conal Elliott's original FRP implementation, unmaintained | —

For comparison, here are a few non-FRP implementations of the same small program.

library | classification | example app
:---:|:---:|:---:
— | callbacks | [code](callback-example/Main.hs)
— | pure functions | [code](gloss-example/Main.hs)
[objective](https://hackage.haskell.org/package/objective) | scenario 0, push-pull automatons | [code](objective-example/Main.hs)

## The TodoMVC of FRP libraries

If you haven't heard of [TodoMVC](http://todomvc.com/), it's a website which hosts several implementation of the same simple Todo app, implemented using various Javascript frameworks and languages. This repository attempts to do the same thing, but with Haskell FRP libraries.

Instead of a Todo app, we use a small task which highlights the area in which FRP libraries differ the most: support for dynamic signal graphs.

### Three scenarios

Evan Czaplicki gave an excellent [presentation about the different categories of FRP libraries](https://www.youtube.com/watch?v=Agu6jipKfYw), in which he categorized FRP libraries according to the choices they make in their attempt to support dynamic graphs. He described the following example:

1. Start with a graph which counts the number of clicks.
1. Click 5 times.
1. Change the graph so that it ignores the clicks.
1. Click 5 more times.
1. Change the graph back to the original graph.

Which number is displayed now? 0, because the new click counter hasn't received any click yet? 5, because the clicks did not reach the click counter while it was outside the graph? 10, because equational reasoning somehow dictates it?

For our toy program, we implement all three scenarios.

## Specification

A [gloss](http://gloss.ouroborus.net) window shall display six buttons, organized as three columns of two buttons. Each column implements one of the above scenarios: the first column chooses 0, the second column chooses 5, and the third column chooses 10. In each column, there is a button whose clicks are counted, and a toggle button to turn the click-counting on and off, starting with on. When off, the counter displays -1.

To be completely precise about the three scenarios:

* The counter for scenario 0 displays the number of clicks since the launch of the app, or since the corresponding toggle button was last toggled on, whichever is most recent.
* The counter for scenario 5 displays the total number of clicks which were received while the corresponding toggle was on.
* The counter for scenario 10 displays the total number of clicks since the launch of the app.

### Static implementation

In order to compare equivalent features in each library, the first half of the implementation should implement the required behaviour using only the core FRP features which are common to all FRP libraries: filtering events, combining current values, and accumulating changes to a local state.

To be completely precise about the expected static graph:

* Each toggle button accumulates `not` operations on a boolean, to establish whether the mode is currently on or off.
* This boolean is used to filter the click events which reach the counter for scenario 5.
* The counters for scenarios 5 and 10 accumulate `(+1)` operations on an integer.
* The counter for scenario 0 accumulates `const 0` and `(+1)` operations, depending on whether the incoming event is a toggle or a click.
* The current counters and modes are combined so that `-1` is used when the mode is off.

### Dynamic implementation

In order to compare the parts of the libraries which differ from each other, this second half of the implementation should reimplement some of the scenarios from part one using dynamic graph modifications, when appropriate. Libraries which only support static graphs should skip this section (I hasten to point out that focusing on static graphs is a perfectly valid point in the design space, as explained in Evan's presentation).

For libraries which do support graph modifications, it is likely that only one of the scenarios can be implemented: temporarily remove the counter from the graph while the toggle is off, then try out the application to see which scenario occurs. In the unlikely case of a library which supports more than one way to modify its graph, more than one scenario might be implementable.

Due to the variability, I cannot give a more precise description of the task, but I do want to point out a common trap: don't reimplement first-order primitives using the higher-order ones. In particular:

* For scenario 5, it is tempting to switch between a graph which generates click events and a graph which doesn't, but this is simply a filter reimplementation. Instead, demonstrate that the events don't reach the counter when it is outside the graph, by switching between two counters for example.
* For scenario 10, it is tempting to separately define a counter which counts all clicks and then to switch between this counter and -1, but this is simply combining the current values of existing signals. Instead, demonstrate that events are recorded and replayed, by hiding the definition of your counter inside a conditional for example.

### Gloss integration

Since the goal is to compare FRP implementations, not GUI systems, a simple implementation of the 6 buttons GUI is provided, along with methods to determine whether the current event is a click or a toggle. Gloss supports both IO-based and state-based APIs, which should make it easy to hook up any FRP library. See the existing implementations for details.

## Classification

Evan's presentation classifies FRP libraries into four categories according to the choices they make regarding dynamic graphs. In our list of implementations at the top of this page, we tag each library with the category it belongs to, as well as the scenarios it can implement via dynamic graph changes. There are also other important distinctions between libraries which have nothing to do with dynamic graphs, whose corresponding tags are described in this section.

* First-order FRP: from Evan's classification, an FRP library which only supports static graphs.
* High-order FRP: from Evan's classification, an FRP library in which event streams are infinite and the graphs can be changed by collapsing a signal of signals of values into a signal of values.
* Asynchronous data flow: from Evan's classification, an FRP library in which fast event-processing nodes may receive more recent events than their slower neighbours. Some versions of this model support "cold" signals, in which the event processing is skipped if nobody is listening for the results.
* Arrowized FRP: from Evan's classification, an FRP library in which graph nodes are automatons which may or may not tick each frame, depending on whether or not they are currently part of the graph. Best for scenario 5. Another way to view this category is that the primary abstraction isn't signals, but functions between signals.
* Events and behaviours: an FRP library in which there are two kinds of reactive objects: behaviours hold a value at every point in time, while events only hold values when the event they represent occurs.
* Signals: an FRP library in which all reactive values hold a value at every point in time. Typically, events are represented via `Maybe`.
* Step signals: a separate representation for signals whose value only changes at specific points in time, typically when an event occurs.
* Continuous: an FRP library [in the style of Conal Elliott](https://github.com/conal/talk-2015-essence-and-origins-of-frp#readme), meaning that signals are functions from time to values. This built-in notion of time allows interpolation between values, and other time-based transformations.


## Contributing

If the example app for your favourite FRP library is missing or non-idiomatic, or if there are other axes of comparison which you think should also be considered, feel free to open an issue or to send a pull request!
