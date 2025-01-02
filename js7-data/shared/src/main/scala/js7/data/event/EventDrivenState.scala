package js7.data.event

import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.Problems.{KeyedEventProblem, OrderCannotAttachedToPlanProblem}
import js7.data.event.EventDrivenState.*
import scala.collection.immutable.Map.Map2
import scala.util.boundary

trait EventDrivenState[Self <: EventDrivenState[Self, E], E <: Event] extends BasicState[Self]:
  this: Self =>

  override def companion: Companion[Self, E]

  def applyKeyedEvent(keyedEvent: KeyedEvent[E]): Checked[Self]

  def applyStampedEvents(stampedEvents: Iterable[Stamped[KeyedEvent[E]]]): Checked[Self] =
    var state = this
    var problem: Problem | Null = null

    boundary:
      for stamped <- stampedEvents.iterator do
        state.applyKeyedEvent(stamped.value) match
          case Left(prblm) =>
            problem = prblm match
              case OrderCannotAttachedToPlanProblem(orderId) if stamped.value.key ==
                orderId => prblm
              case _ =>
                prblm.withPrefix(s"Event '$stamped' cannot be applied to ${companion.name}:")
            boundary.break()
          case Right(s) =>
            state = s

    problem.toLeftOr(state)

  final def applyKeyedEvents(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[Self] =
    var state = this
    var problem: Problem | Null = null

    boundary:
      for keyedEvent <- keyedEvents.iterator do
        state.applyKeyedEvent(keyedEvent) match
          case Left(prblm) =>
            problem = adaptProblem(prblm, keyedEvent)
            boundary.break()
          case Right(s) =>
            state = s

    problem.toLeftOr(state)

  private def adaptProblem(
    prblm: Problem,
    keyedEventOrStamped: KeyedEvent[?] | Stamped[KeyedEvent[?]])
  : Problem =
    val keyedEvent = keyedEventOrStamped match
      case stamped: Stamped[KeyedEvent[?]] => stamped.value
      case ke: KeyedEvent[?] => ke
    prblm match
      case prblm: KeyedEventProblem if prblm.key == keyedEvent.key =>
        prblm

      case _ =>
        prblm.withPrefix(s"Event '$keyedEventOrStamped' cannot be applied to ${companion.name}:")

  protected final def eventNotApplicable(keyedEvent: KeyedEvent[Event]) =
    Left(EventNotApplicableProblem(keyedEvent, this))


object EventDrivenState:

  trait Companion[S <: EventDrivenState[S, E], E <: Event] extends BasicState.Companion[S]:
    given Companion[S, E] = this

    override def toString: String = name


  final case class EventNotApplicableProblem(keyedEvent: KeyedEvent[Event], state: Any)
  extends Problem.Coded:
    def arguments: Map[String, String] = Map2(
      "event", keyedEvent.toString.truncateWithEllipsis(100),
      "state", state.toString.truncateWithEllipsis(100))
