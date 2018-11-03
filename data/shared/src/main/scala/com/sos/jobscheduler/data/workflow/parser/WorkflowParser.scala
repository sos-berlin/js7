package com.sos.jobscheduler.data.workflow.parser

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.{RichTraversable, RichTraversableOnce}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{ExecutablePath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.executable.WorkflowJob
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, Execute, ExplicitEnd, ForkJoin, Goto, If, IfNonZeroReturnCodeGoto, Offer, ReturnCodeMeaning, End ⇒ EndInstr}
import com.sos.jobscheduler.data.workflow.parser.BasicParsers._
import com.sos.jobscheduler.data.workflow.parser.BasicParsers.ops._
import com.sos.jobscheduler.data.workflow.parser.ExpressionParser.booleanExpression
import com.sos.jobscheduler.data.workflow.parser.Parsers.ops._
import com.sos.jobscheduler.data.workflow.{Instruction, Label, Position, Workflow, WorkflowId, WorkflowPath}
import fastparse.all._
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.duration.Duration

/**
  * @author Joacim Zschimmer
  */
object WorkflowParser {

  def parse(string: String): Checked[Workflow] =
    parse(WorkflowPath.NoId, string)

  def parse(id: WorkflowId, string: String): Checked[Workflow] =
    parser.whole.checkedParse(string) map (_.copy(id = id, source = Some(string)))

  private object parser {
    private val label = identifier map Label.apply

    private lazy val workflow = P[Workflow](
      keyword("workflow") ~~/ curlyWorkflow)

    private lazy val curlyWorkflow = P[Workflow](
      curly((labeledInstruction | jobDefinition).rep)
      .flatMap { items ⇒
        val jobs = items.collect { case (name: WorkflowJob.Name, job: WorkflowJob) ⇒ name → job }
        jobs.duplicateKeys(_._1) match {
          case Some(dups) ⇒
            Fail.opaque(s"Duplicate job definitions: ${dups.keys.mkString(", ")}")
          case None ⇒
            CheckedParser(
              Workflow.checked(
                WorkflowPath.NoId,
                items.collect { case o: Instruction.Labeled ⇒ o } .toVector,
                jobs.toMap))
        }
      })

    private val labelDef = P[Label](
      label ~ h ~ ":" ~/ w)

    private val successReturnCodes = P[ReturnCodeMeaning.Success](
      bracketCommaSeq(int)
        map(numbers ⇒ ReturnCodeMeaning.Success(numbers.map(ReturnCode.apply).toSet)))

    private val failureReturnCodes = P[ReturnCodeMeaning.Failure](
      bracketCommaSeq(int)
        map(numbers ⇒ ReturnCodeMeaning.Failure(numbers.map(ReturnCode.apply).toSet)))

    private val endInstruction = P[EndInstr](
      keyword("end").!
        map (_ ⇒ ExplicitEnd))

    private val jsonObject =
      P[Map[String, String]](
        curly(commaSeq(quotedString ~~ ":" ~~/ quotedString))
          map (_.toMap))

    private val anonymousWorkflowExecutable = P[WorkflowJob](
      keyValueMap(Map(
        "executable" → quotedString,
        "agent" → path[AgentPath],
        "arguments" → jsonObject,
        "successReturnCodes" → successReturnCodes,
        "failureReturnCodes" → failureReturnCodes))
      .flatMap { keyToValue ⇒
        for {
          agentPath ← keyToValue[AgentPath]("agent")
          executablePath ← keyToValue[String]("executable") map ExecutablePath.apply
          arguments ← keyToValue[Map[String, String]]("arguments", Map.empty[String, String])
          returnCodeMeaning ← keyToValue.oneOfOr[ReturnCodeMeaning](Set("successReturnCodes", "failureReturnCodes"), ReturnCodeMeaning.Default)
        } yield
          WorkflowJob(agentPath, executablePath, arguments, returnCodeMeaning)
      })

    private val executeInstruction = P[Execute.Anonymous](
      (keyword("execute") ~~ anonymousWorkflowExecutable)
        map Execute.Anonymous.apply)

    private val jobInstruction = P[Execute](
      (keyword("job") ~~ identifier)
        map (name ⇒ Execute.Named(WorkflowJob.Name(name))))

    private val forkInstruction = P[ForkJoin]{
      val orderSuffix = P(quotedString map (o ⇒ Position.BranchId.Named(o)))
      val forkBranch = P[ForkJoin.Branch](
        (orderSuffix ~~ curlyWorkflow)
          map ForkJoin.Branch.fromPair)
      P((keyword("fork") ~~ inParentheses(w ~ forkBranch ~ (comma ~ forkBranch).rep ~ w))
        map { case (branch, more) ⇒ ForkJoin(Vector(branch) ++ more) })
    }

    private val offerInstruction = P[Offer](
      (keyword("offer") ~~ keyValue("orderId", quotedString) ~ comma ~ keyValue("timeout", int))
        map { case (orderId_, duration_) ⇒
          Offer(OrderId(orderId_), Duration(duration_, SECONDS))
        })

    private val awaitInstruction = P[AwaitOrder](
      (keyword("await") ~~ keyValue("orderId", quotedString))
        map (orderId_ ⇒ AwaitOrder(OrderId(orderId_))))

    private val ifInstruction = P[If](
      (keyword("if") ~~ "(" ~~ booleanExpression ~~ ")" ~
        w ~ curlyWorkflow ~
        (w ~ "else" ~~ curlyWorkflow ~ w).?
      ) map { case (expr, then_, else_) ⇒
        If(expr, then_, else_)
      }
    )

    private val ifNonZeroReturnCodeGotoInstruction = P[IfNonZeroReturnCodeGoto](
      (keyword("ifNonZeroReturnCodeGoto") ~~ label)
        map { n ⇒ IfNonZeroReturnCodeGoto(n) })

    private val gotoInstruction: Parser[Goto] =
      P((keyword("goto") ~~ label)
        map { n ⇒ Goto(n) })

    private val instruction: Parser[Instruction] =
      P(awaitInstruction |
        endInstruction |
        executeInstruction |
        forkInstruction |
        gotoInstruction |
        ifInstruction |
        ifNonZeroReturnCodeGotoInstruction |
        jobInstruction |
        offerInstruction)

    private val instructionTerminator = P(w ~ ((";" ~ w) | &("}") | End))

    private val optionalInstructionTerminator = P(instructionTerminator | w)

    private val labeledInstruction = P[Labeled](
      (labelDef.rep ~ instruction ~ instructionTerminator)
        map { case (labels, instruction_) ⇒ Labeled(labels.toImmutableSeq, instruction_)})

    private val jobDefinition = P[(WorkflowJob.Name, WorkflowJob)](
      keyword("define") ~~/ keyword("job") ~~/
        identifier.map(WorkflowJob.Name.apply) ~~/
        curly(executeInstruction ~~ instructionTerminator).map(_.job) ~~/
        optionalInstructionTerminator)

    val whole = w ~/ workflow ~~/ End
  }
}
