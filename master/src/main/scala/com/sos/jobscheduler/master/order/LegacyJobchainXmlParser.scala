package com.sos.jobscheduler.master.order

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.common.scalautil.xmls.ScalaXMLEventReader
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.{ExplicitEnd, Goto, IfNonZeroReturnCodeGoto, Job, ReturnCodeMeaning}
import com.sos.jobscheduler.data.workflow.{JobPath, Label, Workflow, WorkflowPath}
import javax.xml.transform.Source
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
object LegacyJobchainXmlParser {

  def parseXml(folderPath: FolderPath, source: Source): Checked[Workflow] =
    Checked.catchNonFatal {
      ScalaXMLEventReader.parseDocument(source) { eventReader ⇒
        import eventReader._

        eventReader.parseElement("job_chain") {
          val items = forEachStartElement[Seq[Labeled]] {
            case "job_chain_node" ⇒
              parseElement() {
                val label = attributeMap.as[Label]("state")
                attributeMap.get("job") match {
                  case Some(jobPathString) ⇒
                    val jobPath = folderPath.resolve[JobPath](jobPathString)
                    val agentPath = folderPath.resolve[AgentPath](attributeMap("agent"))
                    label @: Job(jobPath, agentPath, ReturnCodeMeaning.NoFailure) ::
                      attributeMap.optionAs[Label]("error_state").map(o ⇒ () @: IfNonZeroReturnCodeGoto(o)).toList :::
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
          Workflow(WorkflowPath.Anonymous, items.values.flatten).reduce
        }
      }
    }
}
