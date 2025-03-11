package js7.data.controller

import js7.base.problem.Checked
import js7.base.time.Timestamp
import js7.data.event.{Event, EventColl, KeyedEvent}

type ControllerEventColl[E <: Event, Ctx] = EventColl[ControllerState, E, Ctx]

object ControllerEventColl:

  def apply[E <: Event](aggregate: ControllerState): ControllerEventColl[E, Unit] =
    EventColl[ControllerState, E, Unit](aggregate, ())

  def apply[E <: Event](aggregate: ControllerState, now: => Timestamp): ControllerEventColl[E, Context] =
    EventColl[ControllerState, E, Context](aggregate, Context(() => now))

  def tryEvents[E <: Event](controllerState: ControllerState)
    (keyedEvents: IterableOnce[KeyedEvent[E]])
  : Checked[Vector[KeyedEvent[E]]] =
    ControllerEventColl.keyedEvents(controllerState)(_.add(keyedEvents))

  def keyedEvents[E <: Event](controllerState: ControllerState)
    (body: ControllerEventColl[E, Unit] => Checked[ControllerEventColl[E, Unit]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(apply[E](controllerState)).map(_.keyedEvents)

  def keyedEvents[E <: Event](controllerState: ControllerState, now: => Timestamp)
    (body: ControllerEventColl[E, Context] => Checked[ControllerEventColl[E, Context]])
  : Checked[Vector[KeyedEvent[E]]] =
    body(apply[E](controllerState, now)).map(_.keyedEvents)


  final case class Context(nowFun: () => Timestamp):
    lazy val now = nowFun()
