package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.Instruction.{ExplicitEnd, Goto, IfErrorGoto, Job, Labeled}
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, Label, Workflow}
import javax.xml.transform.Source
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object LegacyJobchainXmlParser {

  def parseXml(folderPath: FolderPath, source: Source): Workflow =
    ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
      import eventReader._

      eventReader.parseElement("job_chain") {
        val items = forEachStartElement[Seq[Labeled]] {
          case "job_chain_node" ⇒
            parseElement() {
              val label = attributeMap.as[Label]("state")
              attributeMap.get("job") match {
                case Some(jobPathString) ⇒
                  label @: Job(AgentJobPath(folderPath.resolve[AgentPath](attributeMap("agent")), folderPath.resolve[JobPath](jobPathString))) ::
                    attributeMap.optionAs[Label]("error_state").map(o ⇒ () @: IfErrorGoto(o)).toList :::
                    attributeMap.optionAs[Label]("next_state").map(o ⇒ () @: Goto(o)).toList
                case None ⇒
                  label @: ExplicitEnd :: Nil
              }
            }

          case "job_chain_node.end" ⇒
            parseElement() {
              attributeMap.as[Label]("state") @: ExplicitEnd :: Nil
            }
        }
        Workflow(items.values.flatten).reduce
      }
    }
}
