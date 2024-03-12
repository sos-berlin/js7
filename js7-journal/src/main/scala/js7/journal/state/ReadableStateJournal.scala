package js7.journal.state

import cats.effect.IO
import js7.data.event.JournaledState
import js7.journal.watch.EventWatch

trait ReadableStateJournal[S <: JournaledState[S]]:
  protected type State <: JournaledState[State]

  protected val S: JournaledState.Companion[S]

  def unsafeCurrentState(): S

  def eventWatch: EventWatch

  final val state: IO[S] =
    IO(unsafeCurrentState())
