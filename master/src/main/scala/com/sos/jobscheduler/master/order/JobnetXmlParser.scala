package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.SideEffect.ImplicitSideEffect
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.engine2.agent.AgentPath
import com.sos.jobscheduler.data.engine2.order.{JobPath, Jobnet, JobnetPath, NodeId}
import com.sos.jobscheduler.data.folder.FolderPath
import javax.xml.transform.Source
import scala.collection.immutable

/**
  * @author Joacim Zschimmer
  */
object JobnetXmlParser {

  def parseXml(jobnetPath: JobnetPath, source: Source): Jobnet =
    ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
      import eventReader._
      val folderPath = FolderPath.parentOf(jobnetPath)

      eventReader.parseElement("job_chain") {
        val items = forEachStartElement[NodeItem] {
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
                  Node(Jobnet.EndNode(nodeId))
              }
            }
          case "job_chain_node.end" ⇒
            parseElement() {
              Node(Jobnet.EndNode(attributeMap.as[NodeId]("state")))
            }
        }
        val nodes = completeNodes(items.byClass[NodeItem])
        val firstNodeId = nodes.headOption map { _.id } getOrElse {
          throw new IllegalArgumentException("JobChain has no nodes")
        }
        Jobnet(jobnetPath, inputNodeId = firstNodeId, nodes).requireCompleteness
      }
    }

  private def completeNodes(items: immutable.Seq[NodeItem]): immutable.Seq[Jobnet.Node] = {
    var lastNodeId: Option[NodeId] = None
    (for (nodeItem ← items.reverse) yield
      (nodeItem match {
        case Node(node) ⇒ node
        case raw: RawJobNode ⇒
          Jobnet.JobNode(
            raw.nodeId,
            raw.agentPath,
            raw.jobPath,
            onSuccess = raw.next orElse lastNodeId getOrElse {
              throw new IllegalArgumentException("Missing value for attribut next_state=")
            },
            onFailure = raw.error orElse lastNodeId getOrElse {
              throw new IllegalArgumentException("Missing value for attribut error_state=")
            })
      }) sideEffect { _ ⇒
        lastNodeId = Some(nodeItem.nodeId)
      }
    ).reverse
  }

  private sealed trait NodeItem  {
    def nodeId: NodeId
  }

  private case class RawJobNode(nodeId: NodeId, agentPath: AgentPath, jobPath: JobPath, next: Option[NodeId], error: Option[NodeId])
  extends NodeItem

  private case class Node(node: Jobnet.Node) extends NodeItem {
    def nodeId = node.id
  }
}
