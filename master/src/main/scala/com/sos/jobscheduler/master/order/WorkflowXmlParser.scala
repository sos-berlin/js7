package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.utils.Collections.implicits._
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.Workflow.{EndNode, JobNode}
import com.sos.jobscheduler.data.workflow.transition.{ForwardTransition, SuccessFailureTransition, Transition}
import com.sos.jobscheduler.data.workflow.{JobPath, NodeId, Workflow, WorkflowPath}
import javax.xml.transform.Source
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object WorkflowXmlParser {

  def parseXml(workflowPath: WorkflowPath, source: Source): Workflow =
    ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
      import eventReader._
      val folderPath = FolderPath.parentOf(workflowPath)

      eventReader.parseElement("job_chain") {
        val items = forEachStartElement[RawNode] {
          case "job_chain_node" ⇒
            parseElement() {
              val nodeId = attributeMap.as[NodeId]("state")
              attributeMap.get("job") match {
                case Some(jobPathString) ⇒
                  RawJobNode(
                    nodeId,
                    folderPath.resolve[AgentPath](attributeMap("agent")),
                    folderPath.resolve[JobPath](jobPathString),
                    next = attributeMap.optionAs[NodeId]("next_state"),
                    error = attributeMap.optionAs[NodeId]("error_state"))
                case None ⇒
                  RawEndNode(nodeId)
              }
            }
          case "job_chain_node.end" ⇒
            parseElement() {
              RawEndNode(attributeMap.as[NodeId]("state"))
            }
        }
        toWorkflow(workflowPath, items.byClass[RawNode])
      }
    }

  private def toWorkflow(workflowPath: WorkflowPath, rawNodes: Seq[RawNode]): Workflow = {
    val firstNodeId = rawNodes.headOption map { _.id } getOrElse (
      throw new IllegalArgumentException("JobChain has no nodes"))
    Workflow(
        workflowPath,
        start = firstNodeId,
        end = rawNodes.last.id,  // Used only when (possibly?) used as nested workflow
        rawNodes map (_.toNode),
        toTransitions(rawNodes))
      .requireCompleteness
  }

  private def toTransitions(rawNodes: Seq[RawNode]): Seq[Transition] = {
    val resolver = new Resolver(
      idToNode = rawNodes map (_.toNode) toKeyedMap (_.id),
      followers = rawNodes.map(_.id).sliding(2).map(o ⇒ o(0) → o(1)).toMap)
    rawNodes collect { case o: RawJobNode ⇒ resolver.resolve(o) }
  }


  private class Resolver(idToNode: Map[NodeId, Workflow.Node], followers: Map[NodeId, NodeId]) {
    def resolve(rawNode: RawJobNode): Transition = {
      val follower = followers.get(rawNode.id)
      val next = rawNode.next orElse follower getOrElse sys.error(s"Missing attribute next= in JobNode ${rawNode.id}")
      val error = rawNode.error orElse follower getOrElse sys.error(s"Missing attribute error= in JobNode ${rawNode.id}")
      if (next == error)
        Transition(rawNode.id, next, ForwardTransition)
      else
        Transition(Vector(rawNode.id), Vector(next, error), SuccessFailureTransition)
    }
  }

  private sealed trait RawNode {
    def id: NodeId
    def toNode: Workflow.Node
  }

  private case class RawJobNode(id: NodeId, agentPath: AgentPath, jobPath: JobPath, next: Option[NodeId], error: Option[NodeId])
  extends RawNode {
    def toNode = JobNode(id, agentPath, jobPath)
  }

  private case class RawEndNode(id: NodeId) extends RawNode {
    def toNode = EndNode(id)
  }
}
