package com.sos.jobscheduler.data.workflow.parser

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichTraversable, RichTraversableOnce}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, Fork, Goto, If, IfNonZeroReturnCodeGoto, Offer, ReturnCodeMeaning, TryInstruction, End => EndInstr, Fail => FailInstr}
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import com.sos.jobscheduler.data.workflow.parser.BasicParsers.ops._
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser.booleanExpression
import com.sos.jobscheduler.data.workflow.parser.Parsers.ops._
import com.sos.jobscheduler.data.workflow.position.BranchId
import com.sos.jobscheduler.data.workflow.{Instruction, Label, Workflow, WorkflowId, WorkflowPath}
import fastparse.all._
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
object WorkflowParser
{
  def parse(string: String): Checked[Workflow] =
    parse(WorkflowPath.NoId, string)

  def parse(id: WorkflowId, string: String): Checked[Workflow] =
    parser.whole.checkedParse(string) map (_.copy(id = id, source = Some(string)))

  private object parser
  {
    private val label = identifier map Label.apply

    private val instructionTerminator = P((";" ~ w) | &("}") | End)
    //Scala-like: val instructionTerminator = P(h ~ (newline | (";" ~ w) | &("}") | End))

    private lazy val workflowDefinition = P[Workflow](
      keyword("define") ~~/ keyword("workflow") ~~/ curlyWorkflow.flatMap(o ⇒ CheckedParser(o.completelyChecked)))

    private lazy val curlyWorkflow = P[Workflow](
      curly((labeledInstruction | jobDefinition).rep)
      .flatMap { items ⇒
        val jobs = items.collect { case (name: WorkflowJob.Name, job: WorkflowJob) ⇒ name → job }
        jobs.duplicateKeys(_._1) match {
          case Some(dups) ⇒
            Fail.opaque(s"Duplicate job definitions: ${dups.keys.mkString(", ")}")
          case None ⇒
            CheckedParser(
              Workflow.checkedSub(
                WorkflowPath.NoId,
                items.collect { case o: Instruction.Labeled ⇒ o } .toVector,
                jobs.toMap))
        }
      })

    private val labelDef = P[Label](
      label ~ h ~ ":" ~/ w)

    private val returnCode = P[ReturnCode](int map ReturnCode.apply)

    private val successReturnCodes = P[ReturnCodeMeaning.Success](
      bracketCommaSeq(returnCode)
        map(returnCodes ⇒ ReturnCodeMeaning.Success(returnCodes.toSet)))

    private val failureReturnCodes = P[ReturnCodeMeaning.Failure](
      bracketCommaSeq(returnCode)
        map(returnCodes ⇒ ReturnCodeMeaning.Failure(returnCodes.toSet)))

    private val endInstruction = P[EndInstr](
      keyword("end").! ~~/ instructionTerminator
        map (_ ⇒ ExplicitEnd))

    private val arguments =
      P[Arguments](
        curly(commaSeq(quotedString ~~ ":" ~~/ quotedString))
          map (kvs ⇒ Arguments(kvs.toMap)))

    private val anonymousWorkflowExecutable = P[WorkflowJob](
      keyValueMap(Map(
        "executable" → quotedString,
        "agent" → path[AgentPath],
        "arguments" → arguments,
        "successReturnCodes" → successReturnCodes,
        "failureReturnCodes" → failureReturnCodes,
        "taskLimit" → int))
      .flatMap { keyToValue ⇒
        for {
          agentPath ← keyToValue[AgentPath]("agent")
          executablePath ← keyToValue[String]("executable") map ExecutablePath.apply
          arguments ← keyToValue[Arguments]("arguments", Arguments.empty)
          returnCodeMeaning ← keyToValue.oneOfOr[ReturnCodeMeaning](Set("successReturnCodes", "failureReturnCodes"), ReturnCodeMeaning.Default)
          taskLimit ← keyToValue[Int]("taskLimit", WorkflowJob.DefaultTaskLimit)
        } yield
          WorkflowJob(agentPath, executablePath, arguments.toMap, returnCodeMeaning, taskLimit = taskLimit)
      })

    private val executeInstruction = P[Execute.Anonymous](
      (keyword("execute") ~~ anonymousWorkflowExecutable ~~/ instructionTerminator)
        map Execute.Anonymous.apply)

    private val jobInstruction = P[Execute](
      (keyword("job") ~~ identifier ~~
        (comma ~~
          keyValueMap(Map("arguments" → arguments))
        ).?  ~~/ instructionTerminator
      ).flatMap {
        case (name, None) ⇒
          valid(Execute.Named(WorkflowJob.Name(name)))
        case (name, Some(keyToValue)) ⇒
          for (arguments ← keyToValue[Arguments]("arguments", Arguments.empty)) yield
            Execute.Named(WorkflowJob.Name(name), defaultArguments = arguments.toMap)
      })

    private val failInstruction = P[FailInstr](
      (keyword("fail") ~~/ keyValueMap(Map("returnCode" → returnCode)) ~~/ instructionTerminator)
        .flatMap { keyToValue ⇒
          for (returnCode ← keyToValue.get[ReturnCode]("returnCode")) yield
            FailInstr(returnCode)
        })

    private val forkInstruction = P[Fork]{
      val orderSuffix = P(quotedString map (o ⇒ BranchId.Named(o)))
      val forkBranch = P[Fork.Branch](
        (orderSuffix ~~ curlyWorkflow)
          map Fork.Branch.fromPair)
      P((keyword("fork") ~~ inParentheses(w ~ forkBranch ~ (comma ~ forkBranch).rep) ~~/ instructionTerminator)
        map { case (branch, more) ⇒ Fork(Vector(branch) ++ more) })
    }

    private val offerInstruction = P[Offer](
      (keyword("offer") ~~ keyValue("orderId", quotedString) ~ comma ~/ keyValue("timeout", int) ~~/ instructionTerminator)
        map { case (orderId_, duration_) ⇒
          Offer(OrderId(orderId_), Duration(duration_, SECONDS))
        })

    private val awaitInstruction = P[AwaitOrder](
      (keyword("await") ~~ keyValue("orderId", quotedString) ~~/ instructionTerminator)
        map (orderId_ ⇒ AwaitOrder(OrderId(orderId_))))

    private val ifInstruction = P[If](
      (keyword("if") ~~/ "(" ~~ booleanExpression ~~ ")" ~~/
        curlyWorkflow ~/
        (w ~ "else" ~~/ curlyWorkflow).? ~~/ instructionTerminator.?
      ) map { case (expr, then_, else_) ⇒
        If(expr, then_, else_)
      })

    private val tryInstruction = P[TryInstruction](
      (keyword("try") ~~/ curlyWorkflow ~~/ keyword("catch") ~~/ curlyWorkflow ~~/ instructionTerminator.?)
        map { case (try_, catch_) ⇒
          TryInstruction(try_, catch_)
        })

    private val ifNonZeroReturnCodeGotoInstruction = P[IfNonZeroReturnCodeGoto](
      (keyword("ifNonZeroReturnCodeGoto") ~~ label ~~/ instructionTerminator)
        map { n ⇒ IfNonZeroReturnCodeGoto(n) })

    private val gotoInstruction: Parser[Goto] =
      P((keyword("goto") ~~ label ~~/ instructionTerminator)
        map { n ⇒ Goto(n) })

    private val instruction: Parser[Instruction] =
      P(awaitInstruction |
        endInstruction |
        executeInstruction |
        failInstruction |
        forkInstruction |
        gotoInstruction |
        ifInstruction |
        ifNonZeroReturnCodeGotoInstruction |
        jobInstruction |
        tryInstruction |
        offerInstruction)

    private val labeledInstruction = P[Labeled](
      (labelDef.rep ~ instruction)
        map { case (labels, instruction_) ⇒ Labeled(labels.toImmutableSeq, instruction_)})

    private val jobDefinition = P[(WorkflowJob.Name, WorkflowJob)](
      keyword("define") ~~/ keyword("job") ~~/
        identifier.map(WorkflowJob.Name.apply) ~~/
        curly(executeInstruction ~~ instructionTerminator).map(_.job) ~/
        w)

    val whole = w ~/ workflowDefinition ~~/ End
  }

  private case class Arguments(toMap: Map[String, String])
  private object Arguments {
    val empty = new Arguments(Map.empty)
  }
}
