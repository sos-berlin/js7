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

  "matches" - {
    "All" in {
      val q = JobChainQuery.All
      assert(q == JobChainQuery(PathQuery[JobChainPath]("/")))
      assert(q matches QueryableJobChain.ForTest(JobChainPath("/a")))
      assert(q matches QueryableJobChain.ForTest(JobChainPath("/a/b")))
    }

    "Single JobChainPath" in {
      val q = JobChainQuery(PathQuery(JobChainPath("/a/b")))
      assert(q == JobChainQuery(PathQuery[JobChainPath]("/a/b")))
      assert(!(q matches QueryableJobChain.ForTest(JobChainPath("/a"))))
      assert(!(q matches QueryableJobChain.ForTest(JobChainPath("/a/b/c"))))
      assert(q matches QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = false))
      assert(q matches QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = true))
    }

    "isDistributed" in {
      val q = JobChainQuery(PathQuery(FolderPath("/a")), isDistributed = Some(false))
      assert(q matches new QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = false))
      assert(!q.matches(new QueryableJobChain.ForTest(JobChainPath("/a/b"), isDistributed = true)))
    }
  }

  "matchesAllNonDistributed" in {
    assert(JobChainQuery().matchesAllNonDistributed)
    assert(JobChainQuery(isDistributed = None).matchesAllNonDistributed)
    assert(JobChainQuery(isDistributed = Some(false)).matchesAllNonDistributed)
    assert(!JobChainQuery(isDistributed = Some(true)).matchesAllNonDistributed)
    assert(!JobChainQuery(JobChainPath("/a")).matchesAllNonDistributed)
  }

  "toPathAndParameters" in {
    check(
      JobChainQuery(),
      "/" → Map())
    check(
      JobChainQuery(PathQuery[JobChainPath]("/JOB_CHAIN")),
      "/JOB_CHAIN" → Map())
    check(
      JobChainQuery(PathQuery[JobChainPath]("/JOB_CHAIN"), isDistributed = Some(true)),
      "/JOB_CHAIN" → Map("isDistributed" → "true"))

    def check(q: JobChainQuery, pathAndParameters: (String, Map[String, String])): Unit = {
      assert(q.toPathAndParameters == pathAndParameters)
      assert(JobChainQuery.pathAndParameterSerializable.fromPathAndParameters(pathAndParameters) == q)
    }
  }

  "JSON" - {
    "JobChainQuery.All" in {
      check(JobChainQuery.All, "{}")
    }

    "JobChainQuery" in {
      check(JobChainQuery(
        pathQuery = PathQuery(FolderPath("/FOLDER")),
        isDistributed = Some(true)),
        """{
          "path": "/FOLDER/",
          "isDistributed": true
        }""")
    }

    "pathQuery" - {
      "Single JobChainPath" in {
        check(
          JobChainQuery(pathQuery = PathQuery(JobChainPath("/FOLDER/JOBCHAIN"))),
          """{
            "path": "/FOLDER/JOBCHAIN"
          }""")
      }

      "Folder, recursive" in {
        check(
          JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = true)),
          """{
            "path": "/FOLDER/"
          }""")
      }

      "Folder, not recursive" in {
        check(
          JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = false)),
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
