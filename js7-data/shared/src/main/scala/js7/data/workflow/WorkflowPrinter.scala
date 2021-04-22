package js7.data.workflow

import cats.Show
import js7.base.time.ScalaTime._
import js7.base.utils.Identifier
import js7.base.utils.ScalaUtils.syntax._
import js7.data.job.{CommandLineExecutable, InternalExecutable, PathExecutable, ScriptExecutable}
import js7.data.lock.LockPath
import js7.data.value.ValuePrinter.{appendObjectExpression, appendQuoted, appendValue}
import js7.data.value.expression.Expression.ObjectExpression
import js7.data.value.{NamedValues, ValuePrinter}
import js7.data.workflow.WorkflowPrinter._
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fail, Finish, Fork, Gap, Goto, If, IfFailedGoto, ImplicitEnd, LockInstruction, Offer, Retry, ReturnCodeMeaning, TryInstruction}
import scala.annotation.tailrec

/**
  * @author Joacim Zschimmer
  */
final class WorkflowPrinter(sb: StringBuilder) {

  def appendQuoted(string: String) = ValuePrinter.appendQuoted(sb, string)

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

  def appendNamedValues(namedValues: NamedValues): Unit = WorkflowPrinter.appendNamedValues(sb, namedValues)

  def indent(nesting: Int) = for (_ <- 0 until nesting) sb ++= "  "

  def appendWorkflowExecutable(job: WorkflowJob): Unit = {
    sb ++= "agent="
    appendQuoted(job.agentId.string)
    if (job.taskLimit != WorkflowJob.DefaultTaskLimit) {
      sb ++= ", taskLimit="
      sb.append(job.taskLimit)
    }
    if (job.defaultArguments.nonEmpty) {
      sb ++= ", defaultArguments="
      appendNamedValues(job.defaultArguments)
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
    for (o <- job.sigkillDelay) {
      sb.append(", sigkillDelay=")
      sb.append(o.toBigDecimalSeconds)  // TODO Use floating point
    }
    job.executable match {
      case PathExecutable(path, envExpr, v1Compatible) =>
        if (v1Compatible) sb ++= ", v1Compatible=true"
        appendEnv(envExpr)
        sb ++= ", executable="
        appendQuoted(path)

      case ScriptExecutable(script, envExpr, v1Compatible) =>
        if (v1Compatible) sb ++= ", v1Compatible=true"
        appendEnv(envExpr)
        sb ++= ", script="
        appendQuotedExpression(script)  // Last argument, because the script may have multiple lines

      case CommandLineExecutable(command, envExpr) =>
        appendEnv(envExpr)
        sb ++= ", command="
        appendQuoted(command.toString)

      case InternalExecutable(className, jobArguments, arguments) =>
        sb ++= ", internalJobClass="
        appendQuoted(className)
        if (jobArguments.nonEmpty) {
          sb ++= ", jobArguments="
          appendNamedValues(jobArguments)
        }
        if (arguments.nonEmpty) {
          sb ++= ", arguments="
          appendObjectExpression(sb, arguments)
        }
    }
  }

  def appendEnv(env: ObjectExpression): Unit =
    if (env.nonEmpty) {
      sb.append(", env=")
      appendObjectExpression(sb, env)
    }

  def appendWorkflowContent(nesting: Int, workflow: Workflow): Unit = {
    for (labeled <- workflow.labeledInstructions if !labeled.instruction.isInstanceOf[ImplicitEnd]) {
      appendLabeledInstruction(nesting, labeled)
    }

    if (workflow.nameToJob.nonEmpty) sb ++= "\n"
    for ((name, job) <- workflow.nameToJob) {
      indent(nesting)
      sb ++= "define job "
      appendIdentifier(name.string)
      sb ++= " {\n"
      indent(nesting + 1)
      sb ++= "execute "
      appendWorkflowExecutable(job)
      sb ++= "\n"
      indent(nesting)
      sb ++= "}\n"
    }
  }

  def appendLabeledInstruction(nesting: Int, labeled: Instruction.Labeled): Unit = {
    indent(nesting)
    for (label <- labeled.maybeLabel) {
      appendIdentifier(label.string)
      sb ++= ": "
    }
    labeled.instruction match {
      case AwaitOrder(orderId, _) =>
        sb ++= "await orderId="
        appendQuoted(orderId.string)
        sb ++= ";\n"

      case ExplicitEnd(_) =>
        sb ++= "end;\n"

      case Execute.Anonymous(workflowExecutable, arguments, _) =>
        sb ++= "execute "
        appendWorkflowExecutable(workflowExecutable)
        if (arguments.nonEmpty) {
          sb ++= ", defaultArguments="
          appendNamedValues(arguments)
        }
        sb ++= ";\n"

      case Execute.Named(name, defaultArguments, _) =>
        sb ++= "job "
        appendIdentifier(name.string)
        if (defaultArguments.nonEmpty) {
          sb ++= ", defaultArguments="
          appendNamedValues(defaultArguments)
        }
        sb ++= ";\n"

      case Fail(maybeErrorMessage, namedValues, uncatchable, _) =>
        sb ++= "fail"
        if (maybeErrorMessage.isDefined || namedValues.nonEmpty) {
          sb ++= (
            (uncatchable ? "uncatchable=true") ++
            maybeErrorMessage.map(o => "message=" + o.toString) ++
              (namedValues.nonEmpty ? ("namedValues=" + namedValuesToString(namedValues)))
            ).mkString(" (", ", ", ")")
        }
        sb ++= ";\n"

      case LockInstruction(LockPath(lockId), maybeCount, lockedWorkflow, _) =>
        sb ++= "lock (lock="
        appendQuoted(lockId)
        for (n <- maybeCount) {
          sb ++= ", count="
          sb.append(n)
        }
        sb ++= ") {\n"
        indent(nesting + 1)
        appendWorkflowContent(nesting + 1, lockedWorkflow)
        indent(nesting)
        sb ++= "};\n"

      case Finish(_) =>
        sb ++= "finish;\n"

      case Fork(branches, _) =>
        def appendBranch(branch: Fork.Branch) = {
          indent(nesting + 1)
          appendQuoted(branch.id.string)
          sb ++= ": {\n"
          appendWorkflowContent(nesting + 2, branch.workflow)
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
        sb.append("goto ")
        appendIdentifier(label.string)
        sb.append(";\n")

      case IfFailedGoto(label, _) =>
        sb.append("ifFailedGoto ")
        appendIdentifier(label.string)
        sb.append(";\n")

      case If(predicate, thenWorkflow, elseWorkflowOption, _) =>
        sb ++= "if (" ++= predicate.toString ++= ") {\n"
        appendWorkflowContent(nesting + 1, thenWorkflow)
        for (els <- elseWorkflowOption) {
          indent(nesting)
          sb ++= "} else {\n"
          appendWorkflowContent(nesting + 1, els)
        }
        indent(nesting)
        sb ++= "}\n"

      case TryInstruction(tryWorkflow, catchWorkflow, retryDelays, maxTries, _) =>
        sb ++= "try "
        if (retryDelays.isDefined || maxTries.isDefined) {
          sb ++= (
            (for (delays <- retryDelays) yield
              "retryDelays=" + delays.map(_.toBigDecimalSeconds.toString).mkString("[", ", ", "]")) ++
            (for (n <- maxTries) yield "maxTries=" + n.toString)
          ).mkString("(", ", ", ")")
        }
        sb ++= "{\n"
        appendWorkflowContent(nesting + 1, tryWorkflow)
        indent(nesting)
        sb ++= "} catch {\n"
        appendWorkflowContent(nesting + 1, catchWorkflow)
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

  private def appendIdentifier(identifier: String): Unit =
    if (Identifier.isIdentifier(identifier)) {
      sb.append(identifier)
    } else {
      sb.append('`')
      sb.append(identifier)
      sb.append('`')
    }
}

object WorkflowPrinter
{
  implicit val WorkflowShow: Show[Workflow] = w => print(w)

  def print(workflow: Workflow): String = {
    val sb = new StringBuilder(1024)
    sb ++= "define workflow {\n"
    new WorkflowPrinter(sb).appendWorkflowContent(nesting = 1, workflow)
    sb ++= "}\n"
    sb.toString
  }

  def instructionToString(instruction: Instruction): String = {
    val sb = new StringBuilder(1024)
    new WorkflowPrinter(sb).appendLabeledInstruction(nesting = 0, () @: instruction)
    sb.toString
  }

  def namedValuesToString(namedValues: NamedValues) = {
    val sb = new StringBuilder(128)
    appendNamedValues(sb, namedValues)
    sb.toString()
  }

  private def appendNamedValues(sb: StringBuilder, namedValues: NamedValues): Unit = {
    sb ++= "{"
    var needComma = false
    for ((k, v) <- namedValues) {
      if (needComma) sb ++= ", "
      needComma = true
      appendQuoted(sb, k)
      sb ++= ": "
      appendValue(sb, v)
    }
    sb ++= "}"
  }
}
