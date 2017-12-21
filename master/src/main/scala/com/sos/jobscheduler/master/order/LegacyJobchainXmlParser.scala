package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, NodeId, WorkflowScript}
import javax.xml.transform.Source
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object LegacyJobchainXmlParser {

  def parseXml(source: Source, folderPath: FolderPath): WorkflowScript =
    ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
      import eventReader._

      eventReader.parseElement("job_chain") {
        val items = forEachStartElement[Seq[WorkflowScript.Statement]] {
          case "job_chain_node" ⇒
            parseElement() {
              val nodeId = attributeMap.as[NodeId]("state")
              attributeMap.get("job") match {
                case Some(jobPathString) ⇒
                  WorkflowScript.Job(nodeId, AgentJobPath(folderPath.resolve[AgentPath](attributeMap("agent")), folderPath.resolve[JobPath](jobPathString))) ::
                    attributeMap.optionAs[NodeId]("error_state").map(WorkflowScript.OnError.apply).toList :::
                    attributeMap.optionAs[NodeId]("next_state").map(WorkflowScript.Goto.apply).toList
                case None ⇒
                  WorkflowScript.End(nodeId) :: Nil
              }
            }

          case "job_chain_node.end" ⇒
            parseElement() {
              WorkflowScript.End(attributeMap.as[NodeId]("state")) :: Nil
            }
        }
        WorkflowScript(items.values.flatten).reduce
      }
    }
}
