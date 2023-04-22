package js7.journal.state

import js7.data.event.JournaledState
import js7.journal.watch.EventWatch
import monix.eval.Task

trait ReadableStateJournal[S <: JournaledState[S]]
{
  protected type State <: JournaledState[State]

  protected val S: JournaledState.Companion[S]

  def unsafeCurrentState(): S

  def eventWatch: EventWatch

  final val state: Task[S] =
    Task(unsafeCurrentState())
}
