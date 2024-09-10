package js7.subagent.director

import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentBundleId, SubagentId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.subagent.director.SubagentKeeper.{DeterminedSubagentBundle, determineSubagentBundle}

final class SubagentKeeperTest extends OurTestSuite:

  private val agentPath = AgentPath("AGENT")
  private val otherAgentPath = AgentPath("OTHER")
  private val rawOrder = Order(OrderId("A"), WorkflowPath("W") ~ "1" /: Position(1), Order.Ready)
  private val aSubagentBundleId = SubagentBundleId("A")
  private val bSubagentBundleId = SubagentBundleId("B")
  private val subagentId = SubagentId("SUBAGENT")

  "determineSubagentBundle" - {
    "Order.agentToStickyOrder" in:
      assert(rawOrder.agentToStickySubagent(agentPath) == None)

      val ourStickySubagent = Order.StickySubagent(agentPath, None)
      assert(rawOrder
        .copy(stickySubagents = List(ourStickySubagent))
        .agentToStickySubagent(agentPath).get
        .eq(ourStickySubagent))

      assert(rawOrder
        .copy(stickySubagents = List(ourStickySubagent))
        .agentToStickySubagent(otherAgentPath)
        == None)

      val otherStickySubagent = Order.StickySubagent(otherAgentPath, None)

      assert(rawOrder
        .copy(stickySubagents = List(ourStickySubagent, otherStickySubagent))
        .agentToStickySubagent(agentPath).get
        .eq(ourStickySubagent))

      assert(rawOrder
        .copy(stickySubagents = List(otherStickySubagent, ourStickySubagent))
        .agentToStickySubagent(agentPath).get
        .eq(ourStickySubagent))

    "No StickyOrder" - {
      "Job without subagentBundleId" in:
        assert(determineSubagentBundle(rawOrder, agentPath, None) ==
          DeterminedSubagentBundle(None))

      "Job subagentBundleId" in:
        assert(determineSubagentBundle(rawOrder, agentPath, Some(bSubagentBundleId)) ==
          DeterminedSubagentBundle(Some(bSubagentBundleId)))
    }

    "StickyOrder for other Agent is ignored" - {
      val order = rawOrder.copy(
        stickySubagents = List(Order.StickySubagent(otherAgentPath, Some(aSubagentBundleId))))

      "Job without subagentBundleId" in:
        assert(determineSubagentBundle(order, agentPath, None) ==
          DeterminedSubagentBundle(None))

      "Job subagentBundleId" in:
        assert(determineSubagentBundle(order, agentPath, Some(bSubagentBundleId)) ==
          DeterminedSubagentBundle(Some(bSubagentBundleId)))
    }

    "StickyOrder is valid for our Agent" - {
      "Yet no stuck Subagent" - {
        "StickyOrder without subagentBundleId" - {
          val order = rawOrder.copy(
            stickySubagents = List(Order.StickySubagent(agentPath, None)))

          "Job without subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, None) ==
              DeterminedSubagentBundle(None, stick = true))

          "Job with subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, Some(bSubagentBundleId)) ==
              DeterminedSubagentBundle(Some(bSubagentBundleId), stick = true))
        }

        "StickyOrder with subagentBundleId" - {
          val order = rawOrder.copy(
            stickySubagents = List(Order.StickySubagent(agentPath, Some(aSubagentBundleId))))

          "Job without subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, None) ==
              DeterminedSubagentBundle(Some(aSubagentBundleId), stick = true))

          "Job with same subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, Some(aSubagentBundleId)) ==
              DeterminedSubagentBundle(Some(aSubagentBundleId), stick = true))

          "Job with different subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, Some(bSubagentBundleId)) ==
              DeterminedSubagentBundle(Some(bSubagentBundleId)))
        }
      }

      "Already a stuck Subagent" - {
        "StickyOrder without subagentBundleId" - {
          val order = rawOrder.copy(
            stickySubagents = List(
              Order.StickySubagent(agentPath, None, stuckSubagentId = Some(subagentId))))

          "Job without subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, None) ==
              DeterminedSubagentBundle.stuck(subagentId))

          "Job with subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, Some(bSubagentBundleId)) ==
              DeterminedSubagentBundle.stuck(subagentId))
        }

        "StickyOrder with subagentBundleId" - {
          val order = rawOrder.copy(
            stickySubagents = List(
              Order.StickySubagent(
                agentPath,
                Some(aSubagentBundleId),
                stuckSubagentId = Some(subagentId))))

          "Job without subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, None) ==
              DeterminedSubagentBundle.stuck(subagentId))

          "Job with same subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, Some(aSubagentBundleId)) ==
              DeterminedSubagentBundle.stuck(subagentId))

          "Job with different subagentBundleId" in:
            assert(determineSubagentBundle(order, agentPath, Some(bSubagentBundleId)) ==
              DeterminedSubagentBundle(Some(bSubagentBundleId)))
        }
      }
    }
  }
