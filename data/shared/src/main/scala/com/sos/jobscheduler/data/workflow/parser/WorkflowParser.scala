package com.sos.jobscheduler.data.workflow.parser

import cats.data.Validated.Valid
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.problem.Checked._
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversable
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.expression.Evaluator
import com.sos.jobscheduler.data.job.{Executable, ExecutablePath, ExecutableScript, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.source.SourcePos
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fork, Goto, If, IfNonZeroReturnCodeGoto, ImplicitEnd, Offer, Retry, ReturnCodeMeaning, TryInstruction, End => EndInstr, Fail => FailInstr}
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser.{booleanExpression, constantExpression}
import com.sos.jobscheduler.data.workflow.parser.Parsers.checkedParse
import com.sos.jobscheduler.data.workflow.{Instruction, Label, Workflow, WorkflowId, WorkflowPath}
import fastparse.NoWhitespace._
import fastparse._
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * @author Joacim Zschimmer
  */
object WorkflowParser
{
  def parse(string: String): Checked[Workflow] =
    parse(WorkflowPath.NoId, string)

  def parse(id: WorkflowId, string: String): Checked[Workflow] =
    checkedParse(string, parser.whole(_))
      .map(_.copy(id = id, source = Some(string)))

  private object parser
  {
    private def label[_: P] = identifier map Label.apply

    private def hardEnd[_: P]: P[Int] =
      Index ~ w ~/ instructionTerminator

    private def instructionTerminator[_: P] = P((";" ~ w) | &("}") | &(keyword("else")) | End)
    //Scala-like: val instructionTerminator = P(h ~ (newline | (";" ~ w) | &("}") | End))

    private def workflowDefinition[_: P] = P[Workflow](
      keyword("define") ~ w ~/ keyword("workflow") ~ w ~/ curlyWorkflow.flatMap(o => checkedToP(o.completelyChecked)))

    private def curlyWorkflow[_: P] = P[Workflow](
      ("{" ~ w ~/ (labeledInstruction | jobDefinition).rep ~ w ~ Index ~ "}" ~ Index)
        .flatMap { case (items, start, end) =>
          val jobs = items.collect { case (name: WorkflowJob.Name, job: WorkflowJob) => name -> job }
          jobs.duplicateKeys(_._1) match {
            case Some(dups) =>
              Fail.opaque(s"unique job definitions (duplicates: ${dups.keys.mkString(", ")})")
            case None =>
              val instructions = items.collect { case o: Instruction.Labeled => o } .toVector
              checkedToP(
                Workflow.checkedSub(
                  WorkflowPath.NoId,
                  if (Workflow.isCorrectlyEnded(instructions)) instructions
                  else instructions :+ (() @: ImplicitEnd(Some(SourcePos(start, end)))),
                  jobs.toMap))
          }
        })

    private def curlyWorkflowOrInstruction[_: P] = P[Workflow](
      curlyWorkflow | instruction.map(o => Workflow.anonymous(Vector(o))))

    private def labelDef[_: P] = P[Label](
      label ~ h ~ ":" ~/ w)

    private def returnCode[_: P] = P[ReturnCode](int map ReturnCode.apply)

    private def successReturnCodes[_: P] = P[ReturnCodeMeaning.Success](
      bracketCommaSequence(returnCode)
        map(returnCodes => ReturnCodeMeaning.Success(returnCodes.toSet)))

    private def failureReturnCodes[_: P] = P[ReturnCodeMeaning.Failure](
      bracketCommaSequence(returnCode)
        map(returnCodes => ReturnCodeMeaning.Failure(returnCodes.toSet)))

    private def endInstruction[_: P] = P[EndInstr](
      Index ~ keyword("end") ~ hardEnd
        map { case (start, end) => ExplicitEnd(sourcePos(start, end)) })

    private def arguments[_: P]: P[Arguments] =
      P[Arguments](
        curly(nonEmptyCommaSequence(quotedString ~ w ~ ":" ~ w ~/ quotedString))
          map (kvs => Arguments(kvs.toMap)))

    private def anonymousWorkflowExecutable[_: P] = P[WorkflowJob](
      for {
        kv <- keyValues(
          keyValueConvert("executable", quotedString)(o => Valid(ExecutablePath(o))) |
          keyValueConvert("script", constantExpression)(o =>
            Evaluator.Constant.eval(o).flatMap(_.asString).map(v => ExecutableScript(v.string))) |
          keyValue("agent", path[AgentRefPath]) |
          keyValue("arguments", arguments) |
          keyValue("successReturnCodes", successReturnCodes) |
          keyValue("failureReturnCodes", failureReturnCodes) |
          keyValue("taskLimit", int))
        agentRefPath <- kv[AgentRefPath]("agent")
        executable <- kv.oneOf[Executable]("executable", "script").map(_._2)
        arguments <- kv[Arguments]("arguments", Arguments.empty)
        returnCodeMeaning <- kv.oneOfOr(Set("successReturnCodes", "failureReturnCodes"), ReturnCodeMeaning.Default)
        taskLimit <- kv[Int]("taskLimit", WorkflowJob.DefaultTaskLimit)
      } yield
        WorkflowJob(agentRefPath, executable, arguments.toMap, returnCodeMeaning, taskLimit = taskLimit))

    private def executeInstruction[_: P] = P[Execute.Anonymous](
      (Index ~ keyword("execute") ~ w ~ anonymousWorkflowExecutable ~ hardEnd)
        .map { case (start, job, end) => Execute.Anonymous(job, sourcePos(start, end)) })

    private def jobInstruction[_: P] = P[Execute](
      (Index ~ keyword("job") ~ w ~ identifier ~ (w ~ comma ~ keyValues(keyValue("arguments", arguments))).? ~ hardEnd)
        .flatMap {
          case (start, name, None, end) =>
            valid(Execute.Named(WorkflowJob.Name(name), sourcePos = sourcePos(start, end)))
          case (start, name, Some(keyToValue), end) =>
            for (arguments <- keyToValue[Arguments]("arguments", Arguments.empty)) yield
              Execute.Named(WorkflowJob.Name(name), defaultArguments = arguments.toMap, sourcePos(start, end))
        })

    private def failInstruction[_: P] = P[FailInstr](
      (Index ~ keyword("fail") ~ keyValues(keyValue("returnCode", returnCode)) ~ hardEnd)
        .flatMap { case (start, keyToValue, end) =>
          for (returnCode <- keyToValue.get[ReturnCode]("returnCode")) yield
            FailInstr(returnCode, sourcePos(start, end))
        })

    private def forkInstruction[_: P] = P[Fork]{
      def branchId = P(quotedString map (o => Fork.Branch.Id(o)))
      def forkBranch = P[Fork.Branch](
        (branchId ~ w ~ ":" ~ w ~ curlyWorkflowOrInstruction)
          map Fork.Branch.fromPair)
      (Index ~ keyword("fork") ~ Index ~ w ~ curly(w ~ forkBranch ~ (comma ~ forkBranch).rep) ~ w ~ instructionTerminator.?)
        .map { case (start, end, (branch, more)) => Fork(Vector(branch) ++ more, sourcePos(start, end)) }
    }

    private def offerInstruction[_: P] = P[Offer](
      (Index ~ keyword("offer") ~ w ~
        specificKeyValue("orderId", quotedString) ~ comma ~/
        specificKeyValue("timeout", int) ~
        hardEnd
      ) map { case (start, orderId_, duration_, end) =>
          Offer(OrderId(orderId_), Duration(duration_, SECONDS), sourcePos(start, end))
        })

    private def awaitInstruction[_: P] = P[AwaitOrder](
      (Index ~ keyword("await") ~ w ~ specificKeyValue("orderId", quotedString) ~ hardEnd)
        map { case (start, orderId_, end) => AwaitOrder(OrderId(orderId_), sourcePos(start, end)) })

    private def ifInstruction[_: P] = P[If](
      (Index ~ keyword("if") ~ w ~/ inParentheses(booleanExpression) ~ Index ~
        w ~/ curlyWorkflowOrInstruction ~/
        (w ~ "else" ~ w ~/ curlyWorkflowOrInstruction).? ~
        w ~/ instructionTerminator.?
      ) map { case (start, expr, end, then_, else_) =>
        If(expr, then_, else_, sourcePos(start, end))
      })

    private def retryInstruction[_: P] = P[Retry](
      (Index ~ keyword("retry") ~ hardEnd)
        .map { case (start, end) => Retry(sourcePos(start, end)) })

    private def tryInstruction[_: P] = P[TryInstruction](
      (Index ~ keyword("try") ~
        (w ~ inParentheses(specificKeyValue("retryDelays", bracketCommaSequence(int)))).? ~
        Index ~
        w ~/ curlyWorkflowOrInstruction ~ w ~/
        keyword("catch") ~ w ~/
        curlyWorkflowOrInstruction ~
        w ~/ instructionTerminator.?
      ) .flatMap { case (start, delays, end, try_, catch_) =>
          checkedToP(
            TryInstruction.checked(try_, catch_,
              delays.map(_.toVector.map(FiniteDuration(_, TimeUnit.SECONDS))),
              sourcePos(start, end)))
        })

    private def ifNonZeroReturnCodeGotoInstruction[_: P] = P[IfNonZeroReturnCodeGoto](
      (Index ~ keyword("ifNonZeroReturnCodeGoto") ~ w ~ label ~ hardEnd)
        .map { case (start, n, end) => IfNonZeroReturnCodeGoto(n, sourcePos(start, end)) })

    private def gotoInstruction[_: P] = P[Goto](
      Index ~ keyword("goto") ~ w ~ label ~ hardEnd
        map { case (start, n, end) => Goto(n, sourcePos(start, end)) })

    private def instruction[_: P]: P[Instruction] =
      P(awaitInstruction |
        endInstruction |
        executeInstruction |
        failInstruction |
        forkInstruction |
        gotoInstruction |
        ifInstruction |
        ifNonZeroReturnCodeGotoInstruction |
        jobInstruction |
        retryInstruction |
        tryInstruction |
        offerInstruction)

    private def labeledInstruction[_: P] = P[Labeled](
      (labelDef.? ~ instruction)
        map { case (maybeLabel, instruction_) => Labeled(maybeLabel, instruction_)})

    private def jobDefinition[_: P] = P[(WorkflowJob.Name, WorkflowJob)](
      keyword("define") ~ w ~/ keyword("job") ~ w ~/
        identifier.map(WorkflowJob.Name.apply) ~ w ~/
        curly(executeInstruction ~ w ~ instructionTerminator).map(_.job) ~/
        w)

    def whole[_: P] = P[Workflow](w ~/ workflowDefinition ~ w ~/ End)
  }

  private def sourcePos(start: Int, end: Int) = Some(SourcePos(start, end))

  private case class Arguments(toMap: Map[String, String])
  private object Arguments {
    val empty = new Arguments(Map.empty)
  }
}
