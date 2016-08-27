package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.event._
import com.sos.scheduler.engine.data.job.JobPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._
import com.sos.scheduler.engine.data.events.SchedulerKeyedEventJsonFormat

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedEventTest extends FreeSpec {

  "FileBasedActivated JSON" in {
    check[JobPath](KeyedEvent(FileBasedActivated)(JobPath("/TEST")),
      """{
        "key": "/TEST",
        "TYPE": "FileBasedActivated"
      }""")(JobPath)
  }

  "FileBasedAdded JSON" in {
    check[JobPath](KeyedEvent(FileBasedAdded)(JobPath("/TEST")),
      """{
        "key": "/TEST",
        "TYPE": "FileBasedAdded"
      }""")
  }

  "FileBasedReplaced JSON" in {
    check[JobPath](KeyedEvent(FileBasedReplaced)(JobPath("/TEST")),
      """{
        "key": "/TEST",
        "TYPE": "FileBasedReplaced"
      }""")
  }

  "FileBasedRemoved JSON" in {
    check[JobPath](KeyedEvent(FileBasedRemoved)(JobPath("/TEST")),
      """{
        "key": "/TEST",
        "TYPE": "FileBasedRemoved"
      }""")
  }

  private def check[A <: TypedPath: TypedPath.Companion](event: AnyKeyedEvent, json: String): Unit = {
    val jsValue = json.parseJson
    assert(event.toJson == jsValue)
    val KeyedEvent(unknownTypedPath: UnknownTypedPath, e: FileBasedEvent) = jsValue.convertTo[AnyKeyedEvent]
    assert(event == KeyedEvent[FileBasedEvent](unknownTypedPath.asTyped[A], e))
  }
}
