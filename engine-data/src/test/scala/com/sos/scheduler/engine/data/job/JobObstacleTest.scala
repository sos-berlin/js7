package com.sos.scheduler.engine.data.job

import com.sos.scheduler.engine.data.filebased.{FileBasedObstacle, FileBasedState}
import com.sos.scheduler.engine.data.job.JobObstacle._
import com.sos.scheduler.engine.data.lock.LockPath
import com.sos.scheduler.engine.data.processclass.ProcessClassObstacle
import java.time.Instant
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobObstacleTest extends FreeSpec {

  "FileBasedObstacles" in {
    check(
      FileBasedObstacles(Set(FileBasedObstacle.BadState(FileBasedState.undefined))),
      """{
        "TYPE": "FileBasedObstacles",
        "fileBasedObstacles": [
          {
            "TYPE": "BadState",
            "state": "undefined"
          }
        ]
      }""")
  }

  "ProcessClassObstacles" in {
    check(
      ProcessClassObstacles(Set(ProcessClassObstacle.ProcessLimitReached(10))),
      """{
        "TYPE": "ProcessClassObstacles",
        "processClassObstacles": [
          {
            "TYPE": "ProcessLimitReached",
            "limit": 10
          }
        ]
      }""")
  }

  "Stopped" in {
    check(
      Stopped,
      """{
        "TYPE": "Stopped"
      }""")
  }

  "BadState" in {
    check(
      BadState(JobState.stopping),
      """{
        "TYPE": "BadState",
        "jobState": "stopping"
      }""")
  }

  "TaskLimitReached" in {
    check(
      TaskLimitReached(10),
      """{
        "TYPE": "TaskLimitReached",
        "limit": 10
      }""")
  }

  "WaitingForProcessClass" in {
    check(
      WaitingForProcessClass,
      """{
        "TYPE": "WaitingForProcessClass"
      }""")
  }

  "NoRunTime" in {
    check(
      NoRuntime(Some(Instant.parse("2016-09-13T11:22:33Z"))),
      """{
        "TYPE": "NoRuntime",
        "plannedAt": "2016-09-13T11:22:33Z"
      }""")
  }

  "WaitingForLocks" in {
    check(
      WaitingForLocks(Set(LockPath("/A-LOCK"), LockPath("/B-LOCK"))),
      """{
        "TYPE": "WaitingForLocks",
        "lockPaths": [
          "/A-LOCK",
          "/B-LOCK"
        ]
      }""")
  }

  private def check(q: JobObstacle, json: String) = {
    assert(q.toJson == json.parseJson)
    assert(json.parseJson.convertTo[JobObstacle] == q)
  }
}
