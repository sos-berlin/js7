package com.sos.jobscheduler.data.workflow

import cats.Show
import com.sos.jobscheduler.base.circeutils.CirceUtils.CompactPrinter
import com.sos.jobscheduler.base.time.Times._
import com.sos.jobscheduler.data.job.{ExecutablePath, ExecutableScript}
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fail, Fork, Gap, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Offer, Retry, ReturnCodeMeaning, TryInstruction}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
object WorkflowPrinter
{
  implicit val WorkflowShow: Show[Workflow] = w => print(w)
  private val JsonPrinter = CompactPrinter.copy(colonRight = " ", objectCommaRight = " ", arrayCommaRight = " ")

  def print(workflow: Workflow): String = {
    val sb = new StringBuilder(1000)
    sb ++= "define workflow {\n"
    appendWorkflowContent(sb, nesting = 1, workflow)
    sb ++= "}\n"
    sb.toString
  }

  private def appendWorkflowContent(sb: StringBuilder, nesting: Int, workflow: Workflow): String = {
    def appendQuoted(string: String) =
      sb.append('"').append(string.replace("\\", "\\\\").replace("\n", "\\n").replace("\"", "\\\"").replace("$", "\\$")).append('"')

    def appendQuotedExpression(string: String) =
      if (string.contains('\n')) {
        @tailrec def q(quote: String): Unit =
          if (string contains quote) q(quote + "'")
          else sb.append('\n')
            .append(quote)
            .append(string.split('\n').map(o => o + "\n" + " "*(quote.length - 1) + "|").mkString)
            .append(quote)
            .append(".stripMargin")
        if (!string.contains('\'')) sb.append('\'').append(string).append('\'')
        else q("''")
      } else appendQuoted(string)

    def indent(nesting: Int) = for (_ <- 0 until nesting) sb ++= "  "

    def appendJsonObject(obj: Map[String, String]): Unit = {
      sb ++= "{"
      var needComma = false
      for ((k, v) <- obj) {
        if (needComma) sb ++= ", "
        needComma = true
        appendQuoted(k)
        sb ++= ": "
        appendQuoted(v)
      }
      sb ++= "}"
    }

    def appendWorkflowExecutable(job: WorkflowJob): Unit = {
      sb ++= "agent="
      appendQuoted(job.agentRefPath.string)
      if (job.taskLimit != WorkflowJob.DefaultTaskLimit) {
        sb ++= ", taskLimit="
        sb.append(job.taskLimit)
      }
      if (job.defaultArguments.nonEmpty) {
        sb ++= ", arguments="
        appendJsonObject(job.defaultArguments)
      }
      job.returnCodeMeaning match {
        case ReturnCodeMeaning.Default =>
        case ReturnCodeMeaning.Success(returnCodes) =>
          sb ++= ", successReturnCodes=["
          sb ++= returnCodes.map(_.number).toVector.sorted.mkString(", ")
          sb += ']'
        case ReturnCodeMeaning.Failure(returnCodes) =>
          sb ++= ", failureReturnCodes=["
          sb ++= returnCodes.map(_.number).toVector.sorted.mkString(", ")
          sb += ']'
      }
      job.executable match {
        case ExecutablePath(path) =>
          sb ++= ", executable="
          appendQuoted(path)
        case ExecutableScript(script) =>
          sb ++= ", script="
          appendQuotedExpression(script)  // Last argument, because the script may have multiple lines
      }
    }

    for (labelled <- workflow.labeledInstructions if !labelled.instruction.isInstanceOf[ImplicitEnd]) {
      indent(nesting)
      for (label <- labelled.labels) {
        sb ++= label.string
        sb ++= ": "
      }
      labelled.instruction match {
        case AwaitOrder(orderId, _) =>
          sb ++= "await orderId="
          appendQuoted(orderId.string)
          sb ++= ";\n"

        case ExplicitEnd(_) =>
          sb ++= "end;\n"

        case Execute.Anonymous(workflowExecutable, _) =>
          sb ++= "execute "
          appendWorkflowExecutable(workflowExecutable)
          sb ++= ";\n"

        case Execute.Named(name, arguments, _) =>
          sb ++= "job "
          sb ++= name.string
          if (arguments.nonEmpty) {
            sb ++= ", arguments="
            appendJsonObject(arguments)
          }
          sb ++= ";\n"

        case Fork(branches, _) =>
          def appendBranch(branch: Fork.Branch) = {
            indent(nesting + 1)
            appendQuoted(branch.id.string)
            sb ++= ": {\n"
            appendWorkflowContent(sb, nesting + 2, branch.workflow)
            indent(nesting + 1)
            sb += '}'
          }

          sb ++= "fork {\n"
          for (b <- branches.take(branches.length - 1)) {
            appendBranch(b)
            sb.append(",\n")
          }
          appendBranch(branches.last)
          sb += '\n'
          indent(nesting)
          sb ++= "};\n"

        case Gap(_) =>
          sb ++= "/*gap*/\n"

        case Goto(label, _) =>
          sb ++= "goto "++= label.string ++= ";\n"

        case Fail(maybeReturnCode, _) =>
          sb ++= "fail"
          for (o <- maybeReturnCode) sb ++= " returnCode=" ++= o.number.toString
          sb ++= ";\n"

        case IfNonZeroReturnCodeGoto(label, _) =>
          sb ++= "ifNonZeroReturnCodeGoto " ++= label.string ++= ";\n"

        case If(predicate, thenWorkflow, elseWorkflowOption, _) =>
          sb ++= "if (" ++= predicate.toString ++= ") {\n"
          appendWorkflowContent(sb, nesting + 1, thenWorkflow)
          for (els <- elseWorkflowOption) {
            indent(nesting)
            sb ++= "} else {\n"
            appendWorkflowContent(sb, nesting + 1, els)
          }
          indent(nesting)
          sb ++= "}\n"

        case TryInstruction(tryWorkflow, catchWorkflow, retryDelays, _) =>
          sb ++= "try "
          if (retryDelays.nonEmpty) {
            sb ++= "(retryDelays="
            sb ++= retryDelays.map(_.toBigDecimalSeconds.toString).mkString("[", ", ", "]")
            sb ++= ") "
          }
          sb ++= "{\n"
          appendWorkflowContent(sb, nesting + 1, tryWorkflow)
          indent(nesting)
          sb ++= "} catch {\n"
          appendWorkflowContent(sb, nesting + 1, catchWorkflow)
          indent(nesting)
          sb ++= "}\n"

        case Retry(_) =>
          sb ++= "retry"
          sb ++= "\n"

        case Offer(orderId, timeout, _) =>
          sb ++= s"offer orderId="
          appendQuoted(orderId.string)
          sb ++= ", timeout="
          sb.append(timeout.toSeconds)
          sb ++= ";\n"
      }
    }

    if (workflow.nameToJob.nonEmpty) sb ++= "\n"
    for ((name, job) <- workflow.nameToJob) {
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
