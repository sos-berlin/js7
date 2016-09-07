package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.filebased.FileBasedObstacle
import com.sos.scheduler.engine.data.jobchain.JobChainObstacle._
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobChainObstacleTest extends FreeSpec {

  "FileBasedObstacle" in {
    val o: JobChainObstacle = FileBasedObstacles(Set(FileBasedObstacle.Missing))
    val json = """{
        "TYPE":"FileBasedObstacles",
        "fileBasedObstacles": [
         {
           "TYPE": "Missing"
         }
        ]
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[JobChainObstacle] == o)
  }

  "Stopped" in {
    val o: JobChainObstacle = Stopped
    val json = """{
        "TYPE": "Stopped"
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[JobChainObstacle] == o)
  }

  "OrderLimitReached" in {
    val o: JobChainObstacle = OrderLimitReached(limit = 10)
    val json = """{
        "TYPE": "OrderLimitReached",
        "limit": 10
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[JobChainObstacle] == o)
  }
}
