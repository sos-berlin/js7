package com.sos.jobscheduler.data.workflow.parser

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, ExplicitEnd, ForkJoin, Goto, If, IfNonZeroReturnCodeGoto, Job, Offer, ReturnCodeMeaning, End ⇒ EndInstr}
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
      (keyword("job") ~~ path[JobPath] ~~ "on" ~~ path[AgentPath] ~~ returnCodeMeaning.?)
        map { case (jobPath_, agentPath_, rc) ⇒ Job(jobPath_, agentPath_, rc getOrElse ReturnCodeMeaning.Default) })

    private val endInstruction = P[EndInstr](
      keyword("end").!
        map (_ ⇒ ExplicitEnd))

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
      P(jobInstruction |
        endInstruction |
        forkInstruction |
        offerInstruction |
        awaitInstruction |
        ifInstruction |
        ifNonZeroReturnCodeGotoInstruction |
        gotoInstruction)

    private val instructionTerminator = P(w ~ ((";" ~ w) | &("}") | End))

    private val labeledInstruction = P[Labeled](
      (labelDef.rep ~ instruction  ~ instructionTerminator)
        map { case (labels, instruction_) ⇒ Labeled(labels.toImmutableSeq, instruction_)})

    val whole = w ~ workflow ~~ End
  }
}
