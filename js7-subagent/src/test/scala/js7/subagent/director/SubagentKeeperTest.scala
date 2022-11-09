package js7.subagent.director

import js7.base.test.OurTestSuite
import js7.data.agent.AgentPath
import js7.data.order.{Order, OrderId}
import js7.data.subagent.{SubagentId, SubagentSelectionId}
import js7.data.workflow.WorkflowPath
import js7.data.workflow.position.Position
import js7.subagent.director.SubagentKeeper.{DeterminedSubagentSelection, determineSubagentSelection}

final class SubagentKeeperTest extends OurTestSuite
{
  private val agentPath = AgentPath("AGENT")
  private val otherAgentPath = AgentPath("OTHER")
  private val rawOrder = Order(OrderId("A"), WorkflowPath("W") ~ "1" /: Position(1), Order.Ready)
  private val aSubagentSelectionId = SubagentSelectionId("A")
  private val bSubagentSelectionId = SubagentSelectionId("B")
  private val subagentId = SubagentId("SUBAGENT")

  "determineSubagentSelection" - {
    "Order.agentToStickyOrder" in {
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
    }

    "No StickyOrder" - {
      "Job without subagentSelectionId" in {
        assert(determineSubagentSelection(rawOrder, agentPath, None) ==
          DeterminedSubagentSelection(None))
      }

      "Job subagentSelectionId" in {
        assert(determineSubagentSelection(rawOrder, agentPath, Some(bSubagentSelectionId)) ==
          DeterminedSubagentSelection(Some(bSubagentSelectionId)))
      }
    }

    "StickyOrder for other Agent is ignored" - {
      val order = rawOrder.copy(
        stickySubagents = List(Order.StickySubagent(otherAgentPath, Some(aSubagentSelectionId))))

      "Job without subagentSelectionId" in {
        assert(determineSubagentSelection(order, agentPath, None) ==
          DeterminedSubagentSelection(None))
      }

      "Job subagentSelectionId" in {
        assert(determineSubagentSelection(order, agentPath, Some(bSubagentSelectionId)) ==
          DeterminedSubagentSelection(Some(bSubagentSelectionId)))
      }
    }

    "StickyOrder is valid for our Agent" - {
      "Yet no stuck Subagent" - {
        "StickyOrder without subagentSelectionId" - {
          val order = rawOrder.copy(
            stickySubagents = List(Order.StickySubagent(agentPath, None)))

          "Job without subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, None) ==
              DeterminedSubagentSelection(None, stick = true))
          }

          "Job with subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, Some(bSubagentSelectionId)) ==
              DeterminedSubagentSelection(Some(bSubagentSelectionId), stick = true))
          }
        }

        "StickyOrder with subagentSelectionId" - {
          val order = rawOrder.copy(
            stickySubagents = List(Order.StickySubagent(agentPath, Some(aSubagentSelectionId))))

          "Job without subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, None) ==
              DeterminedSubagentSelection(Some(aSubagentSelectionId), stick = true))
          }

          "Job with same subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, Some(aSubagentSelectionId)) ==
              DeterminedSubagentSelection(Some(aSubagentSelectionId), stick = true))
          }

          "Job with different subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, Some(bSubagentSelectionId)) ==
              DeterminedSubagentSelection(Some(bSubagentSelectionId)))
          }
        }
      }

      "Already a stuck Subagent" - {
        "StickyOrder without subagentSelectionId" - {
          val order = rawOrder.copy(
            stickySubagents = List(
              Order.StickySubagent(agentPath, None, stuckSubagentId = Some(subagentId))))

          "Job without subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, None) ==
              DeterminedSubagentSelection.stuck(subagentId))
          }

          "Job with subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, Some(bSubagentSelectionId)) ==
              DeterminedSubagentSelection.stuck(subagentId))
          }
        }

        "StickyOrder with subagentSelectionId" - {
          val order = rawOrder.copy(
            stickySubagents = List(
              Order.StickySubagent(
                agentPath,
                Some(aSubagentSelectionId),
                stuckSubagentId = Some(subagentId))))

          "Job without subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, None) ==
              DeterminedSubagentSelection.stuck(subagentId))
          }

          "Job with same subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, Some(aSubagentSelectionId)) ==
              DeterminedSubagentSelection.stuck(subagentId))
          }

          "Job with different subagentSelectionId" in {
            assert(determineSubagentSelection(order, agentPath, Some(bSubagentSelectionId)) ==
              DeterminedSubagentSelection(Some(bSubagentSelectionId)))
          }
        }
      }
    }
  }
}
