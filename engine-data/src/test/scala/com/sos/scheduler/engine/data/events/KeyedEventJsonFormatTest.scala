package com.sos.scheduler.engine.data.events

import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, Event, KeyedEvent}
import com.sos.scheduler.engine.data.filebased.{FileBasedActivated, FileBasedAdded, FileBasedEvent, FileBasedRemoved, FileBasedReplaced}
import com.sos.scheduler.engine.data.job.{JobEvent, JobStateChanged, TaskClosed, TaskEnded, TaskEvent, TaskStarted}
import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
import com.sos.scheduler.engine.data.log.InfoLogged
import com.sos.scheduler.engine.data.order.{JobChainEvent, JobChainNodeActionChanged, JobChainNodeEvent, JobChainStateChanged, JocOrderStatisticsChanged, OrderEvent, OrderFinished, OrderNestedFinished, OrderNestedStarted, OrderNodeChanged, OrderResumed, OrderSetBack, OrderStarted, OrderStepEnded, OrderStepStarted, OrderSuspended}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class KeyedEventJsonFormatTest extends FreeSpec {

  "InfoLogged" in {
    checkJson(KeyedEvent(InfoLogged("MESSAGE-1 text")),
      """{
        "TYPE": "Logged",
        "level": "info",
        "message": "MESSAGE-1 text"
      }""")
  }

  "OrderFinished" in {
    checkJson(KeyedEvent(OrderFinished(NodeId("END")))(JobChainPath("/JOB-CHAIN") orderKey "ORDER-ID"),
      """{
        "TYPE": "OrderFinished",
        "key": "/JOB-CHAIN,ORDER-ID",
        "nodeId": "END"
      }""")
  }

  private def checkJson(event: AnyKeyedEvent, json: String): Unit = {
    assert(SchedulerAnyKeyedEventJsonFormat canSerialize event)
    val jsValue = json.parseJson
    assert(event.toJson(SchedulerAnyKeyedEventJsonFormat) == jsValue)
    assert(event == jsValue.convertTo[AnyKeyedEvent] )
  }

  "SchedulerAnyKeyedEventJsonFormat.typeNameToClass" in {
    // All publicly known event classes
    assert(SchedulerAnyKeyedEventJsonFormat.typeNameToClass == Map(
      "Event" → classOf[Event],
        "FileBasedEvent" → classOf[FileBasedEvent],
          "FileBasedAdded" → FileBasedAdded.getClass,
          "FileBasedReplaced" → FileBasedReplaced.getClass,
          "FileBasedRemoved" → FileBasedRemoved.getClass,
          "FileBasedActivated" → FileBasedActivated.getClass,
        "JobEvent" → classOf[JobEvent],
          "JobStateChanged" → classOf[JobStateChanged],
        "JobChainEvent" → classOf[JobChainEvent],
          "JobChainStateChanged" → classOf[JobChainStateChanged],
          "JobChainNodeActionChanged" → classOf[JobChainNodeActionChanged],
        "JocOrderStatisticsChanged" → classOf[JocOrderStatisticsChanged],
        "TaskEvent" → classOf[TaskEvent],
          "TaskStarted" → TaskStarted.getClass,
          "TaskEnded" → classOf[TaskEnded],
          "TaskClosed" → TaskClosed.getClass,
        "OrderEvent" → classOf[OrderEvent],
          "OrderStarted" → OrderStarted.getClass,
          "OrderFinished" → classOf[OrderFinished],
          "OrderStepStarted" → classOf[OrderStepStarted],
          "OrderStepEnded" → classOf[OrderStepEnded],
          "OrderNodeChanged" → classOf[OrderNodeChanged],
          "OrderNestedStarted" → OrderNestedStarted.getClass,
          "OrderNestedFinished" → OrderNestedFinished.getClass,
          "OrderSetBack" → classOf[OrderSetBack],
          "OrderSuspended" → OrderSuspended.getClass,
          "OrderResumed" → OrderResumed.getClass))
  }
}
