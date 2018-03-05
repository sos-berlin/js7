package com.sos.jobscheduler.core.workflow.notation

import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.problem.{Checked, Problem}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.Identifier.{isIdentifierPart, isIdentifierStart}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.job.{JobPath, ReturnCode}
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import com.sos.jobscheduler.data.workflow.instructions.{AwaitOrder, ExplicitEnd, ForkJoin, Goto, IfNonZeroReturnCodeGoto, IfReturnCode, Job, Offer, ReturnCodeMeaning, End ⇒ EndInstr}
import com.sos.jobscheduler.data.workflow.{Instruction, Label, Position, Workflow, WorkflowId, WorkflowPath}
import fastparse.all._
import java.util.concurrent.TimeUnit.SECONDS
import scala.concurrent.duration.Duration
import scala.reflect.ClassTag

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
    private val inlineComment = {
      val untilStar = P(CharsWhile(_ != '*', min = 0) ~ "*")
      P("/*" ~ untilStar ~ (!"/" ~ untilStar).rep ~ "/")
    }
    private val lineEndComment = P("//" ~ CharsWhile(_ != '\n'))
    private val comment = P(inlineComment | lineEndComment)
    /** Optional whitespace including line ends */
    private val w = P((CharsWhileIn(" \t\r\n") | comment).rep)
    /** Optional horizontal whitespace */
    private val h = P((CharsWhileIn(" \t") | comment).rep)
    private val comma = w ~ "," ~ w
    //private val newline = P(h ~ "\r".? ~ "\n" ~ w)
    //private val commaOrNewLine = P(h ~ ("," | (newline ~ w ~ ",".?)) ~ w)
    private val int = P[Int](("-".? ~ CharsWhile(c ⇒ c >= '0' && c <= '9')).! map (_.toInt))
    private val instructionTerminator = P(w ~ ((";" ~ w) | &("}") | End))
    //Scala-like: private val instructionTerminator = P(h ~ (newline | (";" ~ w) | &("}") | End))

    private val identifier = P((CharPred(isIdentifierStart) ~ CharsWhile(isIdentifierPart, min = 0)).!)
    private val quotedString = P("\"" ~ CharsWhile(c ⇒ c != '"' && c != '\\').! ~ "\"")
    private val label = identifier map Label.apply
    //private val javaClassName = P((identifier ~ ("." ~ identifier).rep).!)

    private val pathString = P[String](quotedString)
    //private val pathString = P(("/" ~ identifier ~ ("/" ~ identifier).rep).!)
    private def path[P <: TypedPath: TypedPath.Companion] = P[P](
      pathString map (p ⇒ FolderPath.Root.resolve[P](p)))

    private lazy val curlyWorkflow: Parser[Workflow] =
      P("{" ~ w ~ workflow ~ w ~ "}")

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
      ("job" ~ w ~ path[JobPath] ~ w ~ "on" ~ w ~ path[AgentPath] ~ w ~ returnCodeMeaning.?)
        map { case (jobPath_, agentPath_, rc) ⇒ Job(jobPath_, agentPath_, rc getOrElse ReturnCodeMeaning.Default) })

    private val endInstruction = P[EndInstr](
      ("end").!
        map (_ ⇒ ExplicitEnd))

    private val forkInstruction = P[ForkJoin]{
      val orderSuffix = P(quotedString map (o ⇒ Position.BranchId.Named(o)))
      val forkBranch = P[ForkJoin.Branch](
        (orderSuffix ~ w ~ curlyWorkflow)
          map ForkJoin.Branch.fromPair)
      P(("fork" ~ w ~ inParentheses(w ~ forkBranch ~ (comma ~ forkBranch).rep ~ w))
        map { case (branch, more) ⇒ ForkJoin(Vector(branch) ++ more) })
    }

    private val offerInstruction = P[Offer](
      ("offer" ~ w ~ keyValue("orderId", quotedString) ~ comma ~ keyValue("timeout", int))
        map { case (orderId_, duration_) ⇒
          Offer(OrderId(orderId_), Duration(duration_, SECONDS))
        })

    private val awaitInstruction = P[AwaitOrder](
      ("await" ~ w ~ keyValue("orderId", quotedString))
        map (orderId_ ⇒ AwaitOrder(OrderId(orderId_))))

    private val ifReturnCodeInstruction = P[IfReturnCode](
      ("if" ~ w ~ "(" ~ w ~ "returnCode" ~ w ~ commaSeq(int) ~ w ~ ")" ~
        w ~ curlyWorkflow ~
        (w ~ "else" ~ w ~ curlyWorkflow ~ w).?
      ) map { case (returnCodes, then_, else_) ⇒
        IfReturnCode(returnCodes.map(o ⇒ ReturnCode(o)).toVector, then_, else_)
      }
    )
    private val ifNonZeroReturnCodeGotoInstruction = P[IfNonZeroReturnCodeGoto](
      ("ifNonZeroReturnCodeGoto" ~ w ~ label)
        map { n ⇒ IfNonZeroReturnCodeGoto(n) })

    private val gotoInstruction: Parser[Goto] =
      P(("goto" ~ w ~ label)
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

    private val labeledInstruction = P[Labeled](
      (labelDef.rep ~ instruction  ~ instructionTerminator)
        map { case (labels, instruction_) ⇒ Labeled(labels.toImmutableSeq, instruction_)})

    val whole = w ~ workflow ~ w ~ End

    private def keyValue[V](name: String, valueParser: Parser[V]): Parser[V] =
      P(name ~ h ~ "=" ~ w ~ valueParser)

    private def inParentheses[A](parser: Parser[A]): Parser[A] =
      P(h ~ "(" ~ w ~ parser ~ w ~ ")")

    private def sequence[A](parser: Parser[A]): Parser[collection.Seq[A]] =
      P("(" ~ commaSeq(parser) ~ ")")

    private def commaSeq[A](parser: Parser[A]): Parser[collection.Seq[A]] =
      P(w ~ parser ~ (comma ~ parser).rep ~ w) map {
        case (head, tail) ⇒ head +: tail
      }

    private def newInstance[A: ClassTag](name: String): A =
      loadClass[A](name).newInstance()

    private def loadClass[A: ClassTag](name: String): Class[A] = {
      val c = Class.forName(name, false, Thread.currentThread.getContextClassLoader).asInstanceOf[Class[A]]
      require(implicitClass[A] isAssignableFrom c, s"Class $name does not implement ${implicitClass[A].getName}")
      c
    }
  }
}
