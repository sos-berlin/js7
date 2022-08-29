package js7.journal.state

import js7.data.event.JournaledState
import js7.journal.watch.EventWatch
import monix.eval.Task

trait ReadableStatePersistence[S <: JournaledState[S]]
{
  protected type State <: JournaledState[State]

  protected val S: JournaledState.Companion[S]

  //TODO def currentState(): S
  def currentState: S

  val state: Task[S] =
    Task(currentState)

  final def awaitCurrentState: Task[S] =
    waitUntilStarted *> state

  def waitUntilStarted: Task[Unit]

  def eventWatch: EventWatch
}
