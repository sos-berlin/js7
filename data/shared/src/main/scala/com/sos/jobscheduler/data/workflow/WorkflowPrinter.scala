package com.sos.jobscheduler.data.workflow

import cats.Show
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, ExplicitEnd, ForkJoin, Gap, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Job, Offer, ReturnCodeMeaning}

/**
  * @author Joacim Zschimmer
  */
object WorkflowPrinter {

  implicit val WorkflowShow: Show[Workflow] = w ⇒ print(w)

  def print(workflow: Workflow): String = {
    val sb = new StringBuilder
    appendWorkflow(sb, 0, workflow)
    sb.toString
  }

  private def appendWorkflow(sb: StringBuilder, nesting: Int, workflow: Workflow): String = {
    def appendQuoted(string: String) = sb.append('"').append(string.replace("\"", "\\\"")).append('"')
    def indent(nesting: Int) = for (_ ← 0 until nesting) sb ++= "  "

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

        case ForkJoin(branches) ⇒
          def appendBranch(branch: ForkJoin.Branch) = {
            indent(nesting + 1)
            appendQuoted(branch.id.string)
            sb ++= " {\n"
            appendWorkflow(sb, nesting + 2, branch.workflow)
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
          appendWorkflow(sb, nesting + 1, thenWorkflow)
          for (els ← elseWorkflowOption) {
            indent(nesting)
            sb ++= "} else {\n"
            appendWorkflow(sb, nesting + 1, els)
          }
          indent(nesting)
          sb ++= "}\n"

        case Job(jobPath, agentPath, returnCodeMeaning) ⇒
          sb ++= "job "
          appendQuoted(jobPath.string)
          sb ++= " on "
          appendQuoted(agentPath.string)
          returnCodeMeaning match {
            case ReturnCodeMeaning.Default ⇒
            case ReturnCodeMeaning.Success(returnCodes) ⇒
              sb ++= " successReturnCodes=("
              sb ++= returnCodes.map(_.number).toVector.sorted.mkString(", ")
              sb += ')'
            case ReturnCodeMeaning.Failure(returnCodes) ⇒
              sb ++= " failureReturnCodes=("
              sb ++= returnCodes.map(_.number).toVector.sorted.mkString(", ")
              sb += ')'
          }
          sb ++= ";\n"

        case Offer(orderId, timeout) ⇒
          sb ++= s"offer orderId="
          appendQuoted(orderId.string)
          sb ++= ", timeout="
          sb.append(timeout.toSeconds)
          sb ++= ";\n"
      }
    }
    sb.toString
  }
}
