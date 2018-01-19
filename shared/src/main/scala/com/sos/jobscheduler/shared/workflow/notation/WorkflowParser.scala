package com.sos.jobscheduler.shared.workflow.notation

import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.Identifier.{isIdentifierPart, isIdentifierStart}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.folder.FolderPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.{AgentJobPath, Instruction, JobPath, Label, Position, Workflow}
import fastparse.all._
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object WorkflowParser {

  def parse(string: String): Either[String, Workflow] =
    parser.whole.parse(string) match {
      case Parsed.Success(result, _) ⇒ Right(result.copy(source = Some(string)))
      case o: Parsed.Failure ⇒ Left(o.toString)
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
        map (stmts ⇒ Workflow(stmts.toVector)))

    private val agentJobPath = P[AgentJobPath](
      ("job" ~ w ~ path[JobPath] ~ w ~ "on" ~ w ~ path[AgentPath])
        map { case (j, a) ⇒ AgentJobPath(a, j) })

    private val labelDef = P[Label](
      label ~ h ~ ":" ~ w)

    private val jobInstruction = P[Instruction.Job](
      agentJobPath
        map Instruction.Job.apply)

    private val endInstruction = P[Instruction.End](
      ("end").!
        map (_ ⇒ Instruction.ExplicitEnd))

    private val forkInstruction = P[Instruction.ForkJoin]{
      val orderSuffix = P(quotedString map (o ⇒ Position.BranchId.Named(OrderId.ChildId(o))))
      val forkBranch = P[Instruction.ForkJoin.Branch](
        (orderSuffix ~ w ~ curlyWorkflow)
          map Instruction.ForkJoin.Branch.fromPair)
      P(("fork" ~ w ~ inParentheses(w ~ forkBranch ~ (comma ~ forkBranch).rep ~ w))
        map { case (branch, more) ⇒ Instruction.ForkJoin(Vector(branch) ++ more) })
    }

    private val ifReturnCodeInstruction = P[Instruction.IfReturnCode](
      ("if" ~ w ~ "(" ~ w ~ "returnCode" ~ w ~ commaSeq(int) ~ w ~ ")" ~
        w ~ curlyWorkflow ~
        (w ~ "else" ~ w ~ curlyWorkflow ~ w).?
      ) map { case (returnCodes, then_, else_) ⇒
          Instruction.IfReturnCode(returnCodes.map(o ⇒ ReturnCode(o)).toVector, Vector(then_) ++ else_)
      }
    )
    private val ifErrorGotoInstruction = P[Instruction.IfErrorGoto](
      ("ifError" ~ w ~ label)
        map { n ⇒ Instruction.IfErrorGoto(n) })

    private val gotoInstruction: Parser[Instruction.Goto] =
      P(("goto" ~ w ~ label)
        map { n ⇒ Instruction.Goto(n) })

    private val instruction: Parser[Instruction] =
      P(jobInstruction |
        endInstruction |
        forkInstruction |
        ifReturnCodeInstruction |
        ifErrorGotoInstruction |
        gotoInstruction)

    private val labeledInstruction = P[Instruction.Labeled](
      (labelDef.rep ~ instruction  ~ instructionTerminator)
        map { case (labels, instruction_) ⇒ Instruction.Labeled(labels.toImmutableSeq, instruction_)})

    val whole = w ~ workflow ~ w ~ End

    private def keyValue[V](name: String, valueParser: Parser[V]): Parser[V] =
      P(name ~ h ~ "=" ~ w ~ valueParser)

    private def inParentheses[A](parser: Parser[A]): Parser[A] =
      P(h ~ "(" ~ w ~ parser ~ w ~ ")")

    private def array[A](parser: Parser[A]): Parser[collection.Seq[A]] =
      P("[" ~ commaSeq(parser) ~ "]")

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
