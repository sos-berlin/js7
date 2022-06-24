package js7.data.workflow

import js7.base.circeutils.CirceUtils.JsonStringInterpolator
import js7.data.agent.AgentPath
import js7.data.item.ItemRevision
import js7.data.workflow.WorkflowPathControlEvent.{WorkflowPathControlAttached, WorkflowPathControlUpdated}
import js7.data.workflow.position.Label
import js7.tester.CirceJsonTester.testJson
import org.scalatest.freespec.AnyFreeSpec

final class WorkflowPathControlEventTest extends AnyFreeSpec
{
  "WorkflowPathControlUpdated" in {
    testJson[WorkflowPathControlEvent](
      WorkflowPathControlUpdated(
        suspended = true,
        Set(Label("LABEL")),
        ItemRevision(123)),
      json"""{
        "TYPE": "WorkflowPathControlUpdated",
        "suspended": true,
        "skip": [ "LABEL" ],
        "revision": 123
       }""")
  }

  "WorkflowControlUpdated 2.4.0-beta.20220621" in {
    assert(
      json"""{
        "TYPE": "WorkflowControlUpdated",
        "suspended": true,
        "revision": 123
       }""".as[WorkflowPathControlEvent] == Right(
        WorkflowPathControlUpdated(
          suspended = true,
          skip = Set.empty,
          revision = ItemRevision(123))))
  }

  "WorkflowPathControlAttached" in {
    testJson[WorkflowPathControlEvent](
      WorkflowPathControlAttached(
        AgentPath("AGENT"),
        ItemRevision(123)),
      json"""{
        "TYPE": "WorkflowPathControlAttached",
        "agentPath": "AGENT",
        "revision": 123
       }""")
  }

  "WorkflowControlAttached 2.4.0-beta.20220621" in {
    assert(
      json"""{
        "TYPE": "WorkflowControlAttached",
        "agentPath": "AGENT",
        "suspended": true,
        "revision": 123
       }""".as[WorkflowPathControlEvent] == Right(
        WorkflowPathControlAttached(
          AgentPath("AGENT"),
          ItemRevision(123))))
  }
}
