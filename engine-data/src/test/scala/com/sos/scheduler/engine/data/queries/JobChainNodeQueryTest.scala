package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.folder.FolderPath
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.{JobChainPath, NodeId}
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobChainNodeQueryTest extends FreeSpec {

  "matchesAllNonDistributed" in {
    assert(JobChainNodeQuery().matchesAllNonDistributed)
    assert(JobChainNodeQuery(JobChainQuery(isDistributed = Some(false))).matchesAllNonDistributed)
    assert(!JobChainNodeQuery(JobChainQuery(isDistributed = Some(true))).matchesAllNonDistributed)
    assert(!JobChainNodeQuery(nodeIds = Some(Set(NodeId("100")))).matchesAllNonDistributed)
    assert(!JobChainNodeQuery(jobPaths = Some(Set(JobPath("/A")))).matchesAllNonDistributed)
  }

  "matchesCompleteJobChains" in {
    assert(JobChainNodeQuery().matchesCompleteJobChains)
    assert(JobChainNodeQuery(JobChainQuery(isDistributed = Some(false))).matchesCompleteJobChains)
    assert(JobChainNodeQuery(JobChainQuery(isDistributed = Some(true))).matchesCompleteJobChains)
    assert(!JobChainNodeQuery(nodeIds = Some(Set(NodeId("100")))).matchesCompleteJobChains)
    assert(!JobChainNodeQuery(jobPaths = Some(Set(JobPath("/A")))).matchesCompleteJobChains)
  }

  "matches" - {
    val node = QueryableJobNode.ForTest(
      QueryableJobChain.ForTest(JobChainPath("/FOLDER/JOB-CHAIN")),
      NodeId("100"),
      JobPath("/JOB"))

    "jobChainQuery JobPath" in {
      assert(JobChainNodeQuery() matches node)
      assert(JobChainNodeQuery(JobChainQuery(JobChainPath("/FOLDER/JOB-CHAIN"))) matches node)
      assert(!(JobChainNodeQuery(JobChainQuery(JobChainPath("/OTHER"))) matches node))
    }

    "jobChainQuery FolderPath" in {
      assert(JobChainNodeQuery() matches node)
      assert(JobChainNodeQuery(JobChainQuery(FolderPath("/FOLDER"))) matches node)
      assert(!(JobChainNodeQuery(JobChainQuery(FolderPath("/OTHER"))) matches node))
    }

    "nodeIds" in {
      assert(JobChainNodeQuery() matches node)
      assert(JobChainNodeQuery(nodeIds = Some(Set(NodeId("100")))) matches node)
      assert(!(JobChainNodeQuery(nodeIds = Some(Set(NodeId("900")))) matches node))
    }

    "jobPaths" in {
      assert(JobChainNodeQuery() matches node)
      assert(JobChainNodeQuery(jobPaths = Some(Set(JobPath("/JOB")))) matches node)
      assert(!(JobChainNodeQuery(jobPaths = Some(Set(JobPath("/OTHER")))) matches node))
    }
  }

  "toPathAndParameters" in {
    check(
      JobChainNodeQuery(),
      "/" → Map())
    check(
      JobChainNodeQuery(JobChainQuery(PathQuery[JobChainPath]("/JOB_CHAIN"))),
      "/JOB_CHAIN" → Map())
    check(
      JobChainNodeQuery(JobChainQuery(PathQuery[JobChainPath]("/JOB_CHAIN"), isDistributed = Some(true))),
      "/JOB_CHAIN" → Map("isDistributed" → "true"))
    check(
      JobChainNodeQuery(
        JobChainQuery(PathQuery[JobChainPath]("/JOB_CHAIN"), isDistributed = Some(true)),
        jobPaths = Some(Set(JobPath("/A"), JobPath("/B"))),
        nodeIds = Some(Set(NodeId("100"), NodeId("200")))),
      "/JOB_CHAIN" → Map(
        "isDistributed" → "true",
        "jobPaths" → "/A,/B",
        "nodeIds" → "100,200"))

    def check(q: JobChainNodeQuery, pathAndParameters: (String, Map[String, String])): Unit = {
      assert(q.toPathAndParameters == pathAndParameters)
      assert(JobChainNodeQuery.pathAndParameterSerializable.fromPathAndParameters(pathAndParameters) == q)
    }
  }

  "JSON" - {
    "JobChainNodeQuery.All" in {
      check(JobChainNodeQuery.All, "{}")
    }

    "JobChainNodeQuery" in {
      check(JobChainNodeQuery(
        jobChainQuery = JobChainQuery(
          pathQuery = PathQuery(FolderPath("/FOLDER")),
          isDistributed = Some(true)),
        nodeIds = Some(Set(NodeId("100"), NodeId("200"))),
        jobPaths = Some(Set(JobPath("/A"), JobPath("/B")))),
        """{
          "path": "/FOLDER/",
          "isDistributed": true,
          "nodeIds": [
            "100",
            "200"
          ],
          "jobPaths": [
            "/A",
            "/B"
          ]
        }""")
    }

    "pathQuery" - {
      "Single JobChainPath" in {
        check(
          JobChainNodeQuery(JobChainQuery(pathQuery = PathQuery(JobChainPath("/FOLDER/JOBCHAIN")))),
          """{
            "path": "/FOLDER/JOBCHAIN"
          }""")
      }

      "Folder, recursive" in {
        check(
          JobChainNodeQuery(JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = true))),
          """{
            "path": "/FOLDER/"
          }""")
      }

      "Folder, not recursive" in {
        check(
          JobChainNodeQuery(JobChainQuery(pathQuery = PathQuery(FolderPath("/FOLDER"), isRecursive = false))),
          """{
            "path": "/FOLDER/*"
          }""")
      }
    }

    def check(q: JobChainNodeQuery, json: String) = {
      assert(q.toJson == json.parseJson)
      assert(json.parseJson.convertTo[JobChainNodeQuery] == q)
    }
  }
}
