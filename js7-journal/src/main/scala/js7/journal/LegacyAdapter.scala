package js7.journal

import cats.effect.IO
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichEitherF
import js7.data.event.{Event, EventCalc, JournaledState, KeyedEvent, Stamped, TimeCtx}
import js7.journal.Journal.{Persist, Persisted}

trait LegacyAdapter[S <: JournaledState[S]]:
  journal: Journal[S] =>

  final def persistKeyedEvent[E <: Event](keyedEvent: KeyedEvent[E])
    (using enclosing: sourcecode.Enclosing)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    IO.defer:
      val E = keyedEvent.event.keyCompanion.asInstanceOf[Event.KeyCompanion[E]]
      persistEvent(using E)(key = keyedEvent.key.asInstanceOf[E.Key], CommitOptions.default): _ =>
        Right(keyedEvent.event)

  final def persistKeyedEvents[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistWithOptions(options)(_ => Right(keyedEvents))

  final def persistKeyedEventsLater[E <: Event](
    keyedEvents: Seq[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[Unit]] =
    journal.persist:
      Persist(
        EventCalc.pure(keyedEvents),
        options.copy(commitLater = true))
    .rightAs(())

  final def persistEvent[E <: Event](using E: Event.KeyCompanion[? >: E])
    (key: E.Key, options: CommitOptions = CommitOptions.default)
    (using enclosing: sourcecode.Enclosing)
  : (S => Checked[E]) => IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    aggregateToEvent =>
      persistEventUnlocked(
        aggregateToEvent.andThen(_.map(event => key.asInstanceOf[event.keyCompanion.Key] <-: event)),
        options)

  private def persistEventUnlocked[E <: Event](
    aggregateToEvent: S => Checked[KeyedEvent[E]],
    options: CommitOptions = CommitOptions.default)
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persistEventCalc[E](
      EventCalc.checked: controllerState =>
        aggregateToEvent(controllerState).map(_ :: Nil),
      options
    ).map(_ map:
      case (Seq(stampedKeyedEvent), updated) => stampedKeyedEvent -> updated)

  final def persist1[E <: Event](aggregateToEvents: S => Checked[KeyedEvent[E]])
  : IO[Checked[(Stamped[KeyedEvent[E]], S)]] =
    persist: aggregate =>
      aggregateToEvents(aggregate).map(_ :: Nil)
    .map(_.map:
      case (Seq(stampedEvent), updated) => stampedEvent -> updated)

  final def persist[E <: Event](keyedEvents: KeyedEvent[E]*): IO[Checked[Persisted[S, E]]] =
    persist_(Persist(EventCalc.pure(keyedEvents)))

  final def persist[E <: Event](aggregateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistWithOptions(CommitOptions.default)(aggregateToEvents)

  private def persistWithOptions[E <: Event](
    options: CommitOptions = CommitOptions.default)
    (aggregateToEvents: S => Checked[Seq[KeyedEvent[E]]])
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    persistEventCalc(
      EventCalc.checked(aggregate => aggregateToEvents(aggregate)),
      options)

  /** Persist multiple events in a transaction. */
  final def persistTransaction[E <: Event](using E: Event.KeyCompanion[? >: E])(key: E.Key)
    (using enclosing: sourcecode.Enclosing)
  : (S => Checked[Seq[E]]) => IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    aggregateToEvents =>
      persistEventCalc(
        EventCalc.checked: aggregate =>
          aggregateToEvents(aggregate)
            .map(_.map(event => key.asInstanceOf[event.keyCompanion.Key] <-: event)),
        CommitOptions.transaction)

  private def persistEventCalc[E <: Event](
    eventCalc: EventCalc[S, E, TimeCtx],
    options: CommitOptions)
  : IO[Checked[(Seq[Stamped[KeyedEvent[E]]], S)]] =
    journal.persist(Persist(eventCalc, options))
      .map(_.map: result =>
        result.stampedKeyedEvents.asInstanceOf[Seq[Stamped[KeyedEvent[E]]]] -> result.aggregate)
