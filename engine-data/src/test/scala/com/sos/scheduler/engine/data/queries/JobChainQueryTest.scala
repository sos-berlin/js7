package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobChainQueryTest extends FreeSpec {

  "All" in {
    val q = JobChainQuery.All
    assert(q == JobChainQuery.Standard(PathQuery[JobChainPath]("/")))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a")))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b")))
  }

  "Single JobChainPath" in {
    val q = JobChainQuery.Standard(PathQuery(JobChainPath("/a/b")))
    assert(q == JobChainQuery.Standard(PathQuery[JobChainPath]("/a/b")))
    assert(!(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a"))))
    assert(!(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b/c"))))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = false))
    assert(q matchesJobChain QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = true))
  }

  "isDistributed" in {
    val q = JobChainQuery(PathQuery(FolderPath("/a")), isDistributed = Some(false))
    assert(q matchesJobChain new QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = false))
    assert(!q.matchesJobChain(new QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = true)))
  }

  "JSON" - {
    "JobChainQuery.All" in {
      check(JobChainQuery.All, "{}")
    }

    "JobChainQuery" in {
      check(JobChainQuery.Standard(
        jobChainPathQuery = PathQuery(FolderPath("/FOLDER")),
        isDistributed = Some(true)),
        """{
          "path": "/FOLDER/",
          "isDistributed": true
        }""")
    }

    "jobChainPathQuery" - {
      "Single JobChainPath" in {
        check(
          JobChainQuery(jobChainPathQuery = PathQuery(JobChainPath("/FOLDER/JOBCHAIN"))),
          """{
            "path": "/FOLDER/JOBCHAIN"
          }""")
      }

      "Folder, recursive" in {
        check(
          JobChainQuery(jobChainPathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = true)),
          """{
            "path": "/FOLDER/"
          }""")
      }

      "Folder, not recursive" in {
        check(
          JobChainQuery(jobChainPathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = false)),
          """{
            "path": "/FOLDER/*"
          }""")
      }
    }

    def check(q: JobChainQuery, json: String) = {
      assert(q.toJson == json.parseJson)
      assert(json.parseJson.convertTo[JobChainQuery] == q)
    }
  }
}
