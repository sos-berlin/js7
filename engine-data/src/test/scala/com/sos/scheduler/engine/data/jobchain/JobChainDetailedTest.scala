package com.sos.scheduler.engine.data.jobchain

import com.sos.scheduler.engine.data.filebased.FileBasedState
import org.junit.runner.RunWith
import org.scalatest.FreeSpec
import org.scalatest.junit.JUnitRunner
import spray.json._

/**
  * @author Joacim Zschimmer
  */
@RunWith(classOf[JUnitRunner])
final class JobChainDetailedTest extends FreeSpec {

  "JSON" in {
    val o = JobChainDetailed(
      JobChainOverview(
        JobChainPath("/TEST"),
        FileBasedState.active),
      List(
        EndNodeOverview(
          NodeKey(
            JobChainPath("/TEST"),
            NodeId("100")))))
    val json = """{
        "overview": {
          "path": "/TEST",
          "fileBasedState": "active",
          "isDistributed": false,
          "obstacles": []
        },
        "nodes": [
          {
            "TYPE": "End",
            "nodeKey": {
              "jobChainPath": "/TEST",
              "nodeId": "100"
            }
          }
        ]
      }""".parseJson
    assert(o.toJson == json)
    assert(json.convertTo[JobChainDetailed] == o)
  }
}
