package com.sos.jobscheduler.data.workflow.parser

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichTraversable, RichTraversableOnce}
import com.sos.jobscheduler.data.agent.AgentRefPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fork, Goto, If, IfNonZeroReturnCodeGoto, Offer, Retry, ReturnCodeMeaning, TryInstruction, End => EndInstr, Fail => FailInstr}
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser.booleanExpression
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

    private def instructionTerminator[_: P] = P((";" ~ w) | &("}") | &(keyword("else")) | End)
    //Scala-like: val instructionTerminator = P(h ~ (newline | (";" ~ w) | &("}") | End))

    private def workflowDefinition[_: P] = P[Workflow](
      keyword("define") ~ w ~/ keyword("workflow") ~ w ~/ curlyWorkflow.flatMap(o => checkedToP(o.completelyChecked)))

    private def curlyWorkflow[_: P] = P[Workflow](
      curly((labeledInstruction | jobDefinition).rep)
        .flatMap { items =>
          val jobs = items.collect { case (name: WorkflowJob.Name, job: WorkflowJob) => name -> job }
          jobs.duplicateKeys(_._1) match {
            case Some(dups) =>
              Fail.opaque(s"unique job definitions (duplicates: ${dups.keys.mkString(", ")})")
            case None =>
              checkedToP(
                Workflow.checkedSub(
                  WorkflowPath.NoId,
                  items.collect { case o: Instruction.Labeled => o } .toVector,
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
      keyword("end").! ~ w ~/ instructionTerminator
        map (_ => ExplicitEnd))

    private def arguments[_: P] =
      P[Arguments](
        curly(nonEmptyCommaSequence(quotedString ~ w ~ ":" ~ w ~/ quotedString))
          map (kvs => Arguments(kvs.toMap)))

    private def anonymousWorkflowExecutable[_: P] = P[WorkflowJob](
      for {
        kv <- keyValues(
          keyValue("executable", quotedString, ExecutablePath.apply) |
          keyValue("agent", path[AgentRefPath]) |
          keyValue("arguments", arguments) |
          keyValue("successReturnCodes", successReturnCodes) |
          keyValue("failureReturnCodes", failureReturnCodes) |
          keyValue("taskLimit", int))
        agentRefPath <- kv[AgentRefPath]("agent")
        executablePath <- kv[ExecutablePath]("executable")
        arguments <- kv[Arguments]("arguments", Arguments.empty)
        returnCodeMeaning <- kv.oneOfOr(Set("successReturnCodes", "failureReturnCodes"), ReturnCodeMeaning.Default)
        taskLimit <- kv[Int]("taskLimit", WorkflowJob.DefaultTaskLimit)
      } yield
        WorkflowJob(agentRefPath, executablePath, arguments.toMap, returnCodeMeaning, taskLimit = taskLimit))

    private def executeInstruction[_: P] = P[Execute.Anonymous](
      (keyword("execute") ~ w ~ anonymousWorkflowExecutable ~ w ~/ instructionTerminator)
        map Execute.Anonymous.apply)

    private def jobInstruction[_: P] = P[Execute](
      (keyword("job") ~ w ~ identifier ~ w ~
        (comma ~ w ~ keyValues(keyValue("arguments", arguments))).? ~ w ~/
        instructionTerminator
      ).flatMap {
        case (name, None) =>
          valid(Execute.Named(WorkflowJob.Name(name)))
        case (name, Some(keyToValue)) =>
          for (arguments <- keyToValue[Arguments]("arguments", Arguments.empty)) yield
            Execute.Named(WorkflowJob.Name(name), defaultArguments = arguments.toMap)
      })

    private def failInstruction[_: P] = P[FailInstr](
      (keyword("fail") ~ w ~/ keyValues(keyValue("returnCode", returnCode)) ~ w ~/ instructionTerminator)
        .flatMap { keyToValue =>
          for (returnCode <- keyToValue.get[ReturnCode]("returnCode")) yield
            FailInstr(returnCode)
        })

    private def forkInstruction[_: P] = P[Fork]{
      def orderSuffix = P(quotedString map (o => Fork.Branch.Id(o)))
      def forkBranch = P[Fork.Branch](
        (orderSuffix ~ w ~ curlyWorkflowOrInstruction)
          map Fork.Branch.fromPair)
      P((keyword("fork") ~ w ~ inParentheses(w ~ forkBranch ~ (comma ~ forkBranch).rep) ~ w ~/ instructionTerminator)
        map { case (branch, more) => Fork(Vector(branch) ++ more) })
    }

    private def offerInstruction[_: P] = P[Offer](
      (keyword("offer") ~ w ~ specificKeyValue("orderId", quotedString) ~ comma ~/ specificKeyValue("timeout", int) ~ w ~/ instructionTerminator)
        map { case (orderId_, duration_) =>
          Offer(OrderId(orderId_), Duration(duration_, SECONDS))
        })

    private def awaitInstruction[_: P] = P[AwaitOrder](
      (keyword("await") ~ w ~ specificKeyValue("orderId", quotedString) ~ w ~/ instructionTerminator)
        map (orderId_ => AwaitOrder(OrderId(orderId_))))

    private def ifInstruction[_: P] = P[If](
      (keyword("if") ~ w ~/ "(" ~ w ~ booleanExpression ~ w ~ ")" ~ w ~/
        curlyWorkflowOrInstruction ~/
        (w ~ "else" ~ w ~/ curlyWorkflowOrInstruction).? ~ w ~/ instructionTerminator.?
      ) map { case (expr, then_, else_) =>
        If(expr, then_, else_)
      })

    private def retryInstruction[_: P] = P[Retry](
      (keyword("retry") ~ w ~/ instructionTerminator)
        .map(_ => Retry))

    private def tryInstruction[_: P] = P[TryInstruction](
      (keyword("try") ~ w ~/
        inParentheses(specificKeyValue("retryDelays", bracketCommaSequence(int))).? ~ w ~/
        curlyWorkflowOrInstruction ~ w ~/
        keyword("catch") ~ w ~/
        curlyWorkflowOrInstruction ~ w ~/
        instructionTerminator.?
        ) .map { case (delays, try_, catch_) =>
            TryInstruction(try_, catch_, delays.getOrElse(Nil).toVector.map(FiniteDuration(_, TimeUnit.SECONDS)))
          })

    private def ifNonZeroReturnCodeGotoInstruction[_: P] = P[IfNonZeroReturnCodeGoto](
      (keyword("ifNonZeroReturnCodeGoto") ~ w ~ label ~ w ~/ instructionTerminator)
        map { n => IfNonZeroReturnCodeGoto(n) })

    private def gotoInstruction[_: P]: P[Goto] =
      P((keyword("goto") ~ w ~ label ~ w ~/ instructionTerminator)
        map { n => Goto(n) })

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
      (labelDef.rep ~ instruction)
        map { case (labels, instruction_) => Labeled(labels.toImmutableSeq, instruction_)})

    private def jobDefinition[_: P] = P[(WorkflowJob.Name, WorkflowJob)](
      keyword("define") ~ w ~/ keyword("job") ~ w ~/
        identifier.map(WorkflowJob.Name.apply) ~ w ~/
        curly(executeInstruction ~ w ~ instructionTerminator).map(_.job) ~/
        w)

    def whole[_: P] = P(w ~/ workflowDefinition ~ w ~/ End)
  }

  private case class Arguments(toMap: Map[String, String])
  private object Arguments {
    val empty = new Arguments(Map.empty)
  }
}
