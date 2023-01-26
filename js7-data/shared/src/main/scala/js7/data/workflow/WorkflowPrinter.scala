package js7.data.workflow

import cats.Show
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.parser.BasicPrinter
import js7.base.time.ScalaTime.*
import js7.base.utils.RangeSet
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.job.{CommandLineExecutable, InternalExecutable, PathExecutable, ProcessExecutable, ReturnCodeMeaning, ShellScriptExecutable}
import js7.data.lock.LockPath
import js7.data.order.OrderEvent.LockDemand
import js7.data.value.ValuePrinter.{appendQuoted, appendValue}
import js7.data.value.expression.Expression
import js7.data.value.{NamedValues, ValuePrinter}
import js7.data.workflow.WorkflowPrinter.*
import js7.data.workflow.instructions.Instructions.jsonCodec
import js7.data.workflow.instructions.executable.WorkflowJob
import js7.data.workflow.instructions.{EmptyInstruction, Execute, ExplicitEnd, Fail, Finish, Fork, Gap, If, ImplicitEnd, LockInstruction, Retry, TryInstruction}
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

  def appendNamedValues(namedValues: NamedValues): Unit =
    WorkflowPrinter.appendNamedValues(sb, namedValues)

  def appendNameToExpression(nameToExpression: Map[String, Expression]): Unit =
    ValuePrinter.appendNameToExpression(sb, nameToExpression)

  def indent(nesting: Int) = for (_ <- 0 until nesting) sb ++= "  "

  def appendWorkflowExecutable(job: WorkflowJob): Unit = {
    sb ++= "agent="
    appendQuoted(job.agentPath.string)
    if (job.parallelism != WorkflowJob.DefaultParallelism) {
      sb ++= ", parallelism="
      sb.append(job.parallelism)
    }
    if (job.defaultArguments.nonEmpty) {
      sb ++= ", defaultArguments="
      appendNameToExpression(job.defaultArguments)
    }
    for (o <- job.sigkillDelay) {
      sb.append(", sigkillDelay=")
      sb.append(o.toBigDecimalSeconds)  // TODO Use floating point
    }
    job.executable match {
      case executable: ProcessExecutable =>
        executable.returnCodeMeaning match {
          case ReturnCodeMeaning.Default =>
          case ReturnCodeMeaning.Success(returnCodes) =>
            sb ++= ", successReturnCodes=["
            sb ++= returnCodes.ranges
              .map {
                case RangeSet.Single(a) => a.number
                case RangeSet.Interval(a, b) => s"${a.number}..${b.number}" // not parseable
              }
              .mkString(", ")
            sb += ']'
          case ReturnCodeMeaning.Failure(returnCodes) =>
            sb ++= ", failureReturnCodes=["
            sb ++= returnCodes.ranges
              .map {
                case RangeSet.Single(a) => a.number
                case RangeSet.Interval(a, b) => s"${a.number}..${b.number}" // not parseable
              }
              .mkString(", ")
            sb += ']'
        }
        executable match {
          case PathExecutable(path, envExpr, _, login, v1Compatible) =>
            if (v1Compatible) sb ++= ", v1Compatible=true"
            appendEnv(envExpr)
            sb ++= ", executable="
            appendQuoted(path)

          case ShellScriptExecutable(script, envExpr, _, login, v1Compatible) =>
            if (v1Compatible) sb ++= ", v1Compatible=true"
            appendEnv(envExpr)
            sb ++= ", script="
            appendQuotedExpression(script)  // Last argument, because the script may have multiple lines

          case CommandLineExecutable(command, envExpr, _, login) =>
            appendEnv(envExpr)
            sb ++= ", command="
            appendQuoted(command.toString)
        }

      case InternalExecutable(className, jobArguments, arguments) =>
        sb ++= ", internalJobClass="
        appendQuoted(className)
        if (jobArguments.nonEmpty) {
          sb ++= ", jobArguments="
          appendNameToExpression(jobArguments)
        }
        if (arguments.nonEmpty) {
          sb ++= ", arguments="
          appendNameToExpression(arguments)
        }
    }
  }

  def appendEnv(env: Map[String, Expression]): Unit =
    if (env.nonEmpty) {
      sb.append(", env=")
      appendNameToExpression(env)
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
      case ExplicitEnd(_) =>
        sb ++= "end;\n"

      case Execute.Anonymous(workflowExecutable, _) =>
        sb ++= "execute "
        appendWorkflowExecutable(workflowExecutable)
        sb ++= ";\n"

      case Execute.Named(name, defaultArguments, _) =>
        sb ++= "job "
        appendIdentifier(name.string)
        if (defaultArguments.nonEmpty) {
          sb ++= ", defaultArguments="
          appendNameToExpression(defaultArguments)
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

      case LockInstruction(Seq(LockDemand(LockPath(lockPath), maybeCount)), lockedWorkflow, _) =>
        sb ++= "lock (lock="
        appendQuoted(lockPath)
        for (n <- maybeCount) {
          sb ++= ", count="
          sb.append(n)
        }
        sb ++= ") {\n"
        indent(nesting + 1)
        appendWorkflowContent(nesting + 1, lockedWorkflow)
        indent(nesting)
        sb ++= "};\n"

      case Finish(_, _) =>
        sb ++= "finish;\n"

      case Fork(branches, agentPath, joinIfFailed, _) =>
        def appendBranch(branch: Fork.Branch) = {
          indent(nesting + 1)
          appendQuoted(branch.id.string)
          sb ++= ": {\n"
          appendWorkflowContent(nesting + 2, branch.workflow)
          indent(nesting + 1)
          sb += '}'
        }

        sb ++= "fork"
        if (joinIfFailed) {
          sb.append(" (")
          sb.append("joinIfFailed=")
          sb.append(joinIfFailed)
          sb.append(')')
        }
        sb ++= " {\n"
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

      case EmptyInstruction(_) =>
        sb ++= "/*empty*/\n"

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

      case instruction =>
        sb ++= instruction.asJson.compactPrint
    }
  }

  private def appendIdentifier(identifier: String): Unit =
    BasicPrinter.appendIdentifier(sb, identifier)
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
