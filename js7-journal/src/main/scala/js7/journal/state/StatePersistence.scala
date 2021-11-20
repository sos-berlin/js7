package js7.journal.state

import js7.data.event.JournaledState

trait StatePersistence[S <: JournaledState[S]]
extends AnyStatePersistence
{
  protected type State = S
}
