package com.sos.jobscheduler.core.workflow.notation

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.core.workflow.notation.BasicParsers._
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, ExplicitEnd, ForkJoin, Goto, IfNonZeroReturnCodeGoto, IfReturnCode, Job, Offer, ReturnCodeMeaning, End ⇒ EndInstr}
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
    parser.whole.parse(string) match {
      case Parsed.Success(result, _) ⇒ Valid(result.copy(id = id, source = Some(string)))
      case o: Parsed.Failure ⇒ Invalid(Problem((o.toString)))
    }

  private object parser {
    private val label = identifier map Label.apply

    private lazy val curlyWorkflow: Parser[Workflow] =
      P(P("{") ~~ workflow ~~ "}")

    private lazy val workflow = P[Workflow](
      labeledInstruction.rep
        map (stmts ⇒ Workflow(WorkflowPath.NoId, stmts.toVector)))

    private val labelDef = P[Label](
      label ~ h ~ ":" ~ w)

    private val successReturnCodes = P[ReturnCodeMeaning.Success](
      sequence(int)
        map(numbers ⇒ ReturnCodeMeaning.Success(numbers.map(ReturnCode.apply).toSet)))

    private val failureReturnCodes = P[ReturnCodeMeaning.Failure](
      sequence(int)
        map(numbers ⇒ ReturnCodeMeaning.Failure(numbers.map(ReturnCode.apply).toSet)))

    private val returnCodeMeaning = P[ReturnCodeMeaning](
      keyValue("successReturnCodes", successReturnCodes) |
      keyValue("failureReturnCodes", failureReturnCodes))

    private val jobInstruction = P[Job](
      (P("job") ~~ path[JobPath] ~~ "on" ~~ path[AgentPath] ~~ returnCodeMeaning.?)
        map { case (jobPath_, agentPath_, rc) ⇒ Job(jobPath_, agentPath_, rc getOrElse ReturnCodeMeaning.Default) })

    private val endInstruction = P[EndInstr](
      ("end").!
        map (_ ⇒ ExplicitEnd))

    private val forkInstruction = P[ForkJoin]{
      val orderSuffix = P(quotedString map (o ⇒ Position.BranchId.Named(o)))
      val forkBranch = P[ForkJoin.Branch](
        (orderSuffix ~~ curlyWorkflow)
          map ForkJoin.Branch.fromPair)
      P((P("fork") ~~ inParentheses(w ~ forkBranch ~ (comma ~ forkBranch).rep ~ w))
        map { case (branch, more) ⇒ ForkJoin(Vector(branch) ++ more) })
    }

    private val offerInstruction = P[Offer](
      (P("offer") ~~ keyValue("orderId", quotedString) ~ comma ~ keyValue("timeout", int))
        map { case (orderId_, duration_) ⇒
          Offer(OrderId(orderId_), Duration(duration_, SECONDS))
        })

    private val awaitInstruction = P[AwaitOrder](
      (P("await") ~~ keyValue("orderId", quotedString))
        map (orderId_ ⇒ AwaitOrder(OrderId(orderId_))))

    private val ifReturnCodeInstruction = P[IfReturnCode](
      (P("if") ~~ "(" ~~ "returnCode" ~~ commaSeq(int) ~~ ")" ~
        w ~ curlyWorkflow ~
        (w ~ "else" ~~ curlyWorkflow ~ w).?
      ) map { case (returnCodes, then_, else_) ⇒
        IfReturnCode(returnCodes.map(o ⇒ ReturnCode(o)).toVector, then_, else_)
      }
    )
    private val ifNonZeroReturnCodeGotoInstruction = P[IfNonZeroReturnCodeGoto](
      (P("ifNonZeroReturnCodeGoto") ~~ label)
        map { n ⇒ IfNonZeroReturnCodeGoto(n) })

    private val gotoInstruction: Parser[Goto] =
      P((P("goto") ~~ label)
        map { n ⇒ Goto(n) })

    private val instruction: Parser[Instruction] =
      P(jobInstruction |
        endInstruction |
        forkInstruction |
        offerInstruction |
        awaitInstruction |
        ifReturnCodeInstruction |
        ifNonZeroReturnCodeGotoInstruction |
        gotoInstruction)

    private val instructionTerminator = P(w ~ ((";" ~ w) | &("}") | End))

    private val labeledInstruction = P[Labeled](
      (labelDef.rep ~ instruction  ~ instructionTerminator)
        map { case (labels, instruction_) ⇒ Labeled(labels.toImmutableSeq, instruction_)})

    val whole = w ~ workflow ~~ End
  }
}
