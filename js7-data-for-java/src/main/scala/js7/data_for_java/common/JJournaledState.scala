package js7.data_for_java.common

import js7.data.event.JournaledState

trait JJournaledState[Self <: JJournaledState[Self, S], S <: JournaledState[S]]
extends JavaWrapper
{
  protected type AsScala = S
}

object JJournaledState
{
  trait Companion[JS <: JJournaledState[JS, S], S <: JournaledState[S]]
  {
    def apply(journaledState: S): JS
  }
}
