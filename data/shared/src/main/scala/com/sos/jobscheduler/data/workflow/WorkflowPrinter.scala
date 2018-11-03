package com.sos.jobscheduler.data.workflow

import cats.Show
import com.sos.jobscheduler.base.circeutils.CirceUtils.CompactPrinter
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, ForkJoin, Gap, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Offer, ReturnCodeMeaning}
import io.circe.syntax.EncoderOps

/**
  * @author Joacim Zschimmer
  */
object WorkflowPrinter {

  implicit val WorkflowShow: Show[Workflow] = w ⇒ print(w)
  private val JsonPrinter = CompactPrinter.copy(colonRight = " ", objectCommaRight = " ", arrayCommaRight = " ")

  def print(workflow: Workflow): String = {
    val sb = new StringBuilder(1000)
    sb ++= "workflow {\n"
    appendWorkflowContent(sb, nesting = 1, workflow)
    sb ++= "}\n"
    sb.toString
  }

  private def appendWorkflowContent(sb: StringBuilder, nesting: Int, workflow: Workflow): String = {
    def appendQuoted(string: String) = sb.append('"').append(string.replace("\"", "\\\"")).append('"')
    def indent(nesting: Int) = for (_ ← 0 until nesting) sb ++= "  "

    def appendWorkflowExecutable(workflowExecutable: WorkflowJob): Unit = {
      sb ++= "executable="
      appendQuoted(workflowExecutable.executablePath.string)
      sb ++= ", agent="
      appendQuoted(workflowExecutable.agentPath.string)
      if (workflowExecutable.defaultArguments.nonEmpty) {
        sb ++= ", arguments="
        sb ++= workflowExecutable.defaultArguments.asJson.pretty(JsonPrinter)
      }
      workflowExecutable.returnCodeMeaning match {
        case ReturnCodeMeaning.Default ⇒
        case ReturnCodeMeaning.Success(returnCodes) ⇒
          sb ++= ", successReturnCodes=["
          sb ++= returnCodes.map(_.number).toVector.sorted.mkString(", ")
          sb += ']'
        case ReturnCodeMeaning.Failure(returnCodes) ⇒
          sb ++= ", failureReturnCodes=["
          sb ++= returnCodes.map(_.number).toVector.sorted.mkString(", ")
          sb += ']'
      }
    }

    for (labelled ← workflow.labeledInstructions if labelled.instruction != ImplicitEnd) {
      indent(nesting)
      for (label ← labelled.labels) {
        sb ++= label.string
        sb ++= ": "
      }
      labelled.instruction match {
        case AwaitOrder(orderId) ⇒
          sb ++= "await orderId="
          appendQuoted(orderId.string)
          sb ++= ";\n"

        case ExplicitEnd ⇒
          sb ++= "end;\n"

        case Execute.Anonymous(workflowExecutable) ⇒
          sb ++= "execute "
          appendWorkflowExecutable(workflowExecutable)
          sb ++= ";\n"

        case Execute.Named(name) ⇒
          sb ++= "job "
          sb ++= name.string
          sb ++= ";\n"

        case ForkJoin(branches) ⇒
          def appendBranch(branch: ForkJoin.Branch) = {
            indent(nesting + 1)
            appendQuoted(branch.id.string)
            sb ++= " {\n"
            appendWorkflowContent(sb, nesting + 2, branch.workflow)
            indent(nesting + 1)
            sb += '}'
          }

          sb ++= "fork (\n"
          for (b ← branches.take(branches.length - 1)) {
            appendBranch(b)
            sb.append(",\n")
          }
          appendBranch(branches.last)
          sb ++= ");\n"

        case Gap ⇒
          sb ++= "/*gap*/\n"

        case Goto(label) ⇒
          sb ++= "goto "++= label.string ++= ";\n"

        case IfNonZeroReturnCodeGoto(label) ⇒
          sb ++= "ifNonZeroReturnCodeGoto " ++= label.string ++= ";\n"

        case If(predicate, thenWorkflow, elseWorkflowOption) ⇒
          sb ++= "if (" ++= predicate.toString ++= ") {\n"
          appendWorkflowContent(sb, nesting + 1, thenWorkflow)
          for (els ← elseWorkflowOption) {
            indent(nesting)
            sb ++= "} else {\n"
            appendWorkflowContent(sb, nesting + 1, els)
          }
          indent(nesting)
          sb ++= "}\n"

        case Offer(orderId, timeout) ⇒
          sb ++= s"offer orderId="
          appendQuoted(orderId.string)
          sb ++= ", timeout="
          sb.append(timeout.toSeconds)
          sb ++= ";\n"
      }
    }

    if (workflow.nameToJob.nonEmpty) sb ++= "\n"
    for ((name, job) ← workflow.nameToJob) {
      indent(nesting)
      sb ++= "define job "
      sb ++= name.string
      sb ++= " {\n"
      indent(nesting + 1)
      sb ++= "execute "
      appendWorkflowExecutable(job)
      sb ++= "\n"
      indent(nesting)
      sb ++= "}\n"
    }
    sb.toString
  }
}
