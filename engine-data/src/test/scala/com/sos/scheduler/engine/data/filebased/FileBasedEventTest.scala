package com.sos.scheduler.engine.data.filebased

import com.sos.scheduler.engine.data.event._
import com.sos.scheduler.engine.data.events.schedulerKeyedEventJsonFormat
import com.sos.scheduler.engine.data.filebaseds.TypedPathRegister
import com.sos.scheduler.engine.data.job.JobPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class FileBasedEventTest extends FreeSpec {

  "FileBasedActivated JSON" in {
    check[JobPath](KeyedEvent(FileBasedActivated)(JobPath("/TEST")),
      """{
        "key": "Job:/TEST",
        "TYPE": "FileBasedActivated"
      }""")(JobPath)
  }

  "FileBasedAdded JSON" in {
    check[JobPath](KeyedEvent(FileBasedAdded)(JobPath("/TEST")),
      """{
        "key": "Job:/TEST",
        "TYPE": "FileBasedAdded"
      }""")
  }

  "FileBasedReplaced JSON" in {
    check[JobPath](KeyedEvent(FileBasedReplaced)(JobPath("/TEST")),
      """{
        "key": "Job:/TEST",
        "TYPE": "FileBasedReplaced"
      }""")
  }

  "FileBasedRemoved JSON" in {
    check[JobPath](KeyedEvent(FileBasedRemoved)(JobPath("/TEST")),
      """{
        "key": "Job:/TEST",
        "TYPE": "FileBasedRemoved"
      }""")
  }

  private def check[A <: TypedPath: TypedPath.Companion](event: AnyKeyedEvent, json: String): Unit = {
    val jsValue = json.parseJson
    implicit def typedPathJsonFormat = TypedPathRegister.WithCompanionJsonFormat

    assert(event.toJson == jsValue)
    val KeyedEvent(typedPath: TypedPath, e: FileBasedEvent) = jsValue.convertTo[AnyKeyedEvent]
    assert(event == KeyedEvent[FileBasedEvent](typedPath.asTyped[A], e))
  }
}
