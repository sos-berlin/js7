package com.sos.scheduler.engine.data.events

import com.sos.scheduler.engine.data.event.{AnyKeyedEvent, KeyedEvent}
import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
import com.sos.scheduler.engine.data.log.InfoLogEvent
import com.sos.scheduler.engine.data.order.OrderFinished
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class EventJsonFormatTest extends FreeSpec {

  "InfoLogEvent" in {
    checkJson(KeyedEvent(InfoLogEvent("MESSAGE-1 text")),
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
    assert(EventJsonFormat canSerialize event)
    val jsValue = json.parseJson
    assert(event.toJson(EventJsonFormat) == jsValue)
    assert(event == jsValue.convertTo[AnyKeyedEvent] )
  }
}
