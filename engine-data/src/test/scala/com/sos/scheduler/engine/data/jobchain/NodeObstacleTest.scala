package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.NodeObstacle._
import java.time.Duration
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class NodeObstacleTest extends FreeSpec {

  "Stopping" in {
    check(
      Stopping,
      """{
        "TYPE": "Stopping"
      }""")
  }

  "Delaying" in {
    check(
      Delaying(Duration ofSeconds 123),
      """{
        "TYPE": "Delaying",
        "duration": 123
      }""")
  }

  "MissingJob" in {
    check(
      MissingJob(JobPath("/JOB")),
      """{
        "TYPE": "MissingJob",
        "jobPath": "/JOB"
      }""")
  }

  "WaitingForJob" in {
    check(
      WaitingForJob,
      """{
        "TYPE": "WaitingForJob"
      }""")
  }

  private def check(q: NodeObstacle, json: String) = {
    assert(q.toJson == json.parseJson)
    assert(json.parseJson.convertTo[NodeObstacle] == q)
  }
}
