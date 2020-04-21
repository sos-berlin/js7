package com.sos.jobscheduler.data.event

import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.data.event.JournaledState.EventNotApplicableProblem

trait EventDrivenState[This <: EventDrivenState[This, E], E <: Event]
{
  this: This =>

  def applyEvent(keyedEvent: KeyedEvent[E]): Checked[This]

  def applyEvents(keyedEvents: IterableOnce[KeyedEvent[E]]): Checked[This] = {
    var state = this
    var problem: Problem = null
    for (keyedEvent <- keyedEvents.iterator if problem == null) {
      state.applyEvent(keyedEvent) match {
        case Left(o) => problem = o withPrefix s"Event '$keyedEvent' cannot be applied:"
        case Right(s) => state = s
      }
    }
    if (problem != null) Left(problem)
    else Right(state)
  }

  protected final def eventNotApplicable(keyedEvent: KeyedEvent[Event]) =
    Left(EventNotApplicableProblem(keyedEvent, this))
}
