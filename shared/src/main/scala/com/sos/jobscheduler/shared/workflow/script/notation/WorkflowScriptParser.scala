package com.sos.jobscheduler.shared.workflow.script.notation

import com.sos.jobscheduler.base.utils.Identifier.{isIdentifierPart, isIdentifierStart}
import com.sos.jobscheduler.base.utils.ScalaUtils.implicitClass
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.filebased.TypedPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.{AgentJobPath, JobPath, NodeId, WorkflowScript}
import fastparse.all._
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

/**
  * @author Joacim Zschimmer
  */
object WorkflowScriptParser {

  def parse(string: String) =
    parser.whole.parse(string) match {
      case Parsed.Success(result, _) ⇒ Right(result.copy(source = Some(string)))
      case o: Parsed.Failure ⇒ Left(o.toString)
    }

  object parser {
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
    private val newline = P(h ~ "\r".? ~ "\n" ~ w)
    private val comma = w ~ "," ~ w
    private val commaOrNewLine = P(h ~ ("," | (newline ~ w ~ ",".?)) ~ w)
    private val statementTerminator = P(w ~ ((";" ~ w) | &("}") | End))
    //Scala-like: private val statementTerminator = P(h ~ (newline | (";" ~ w) | &("}") | End))

    private val identifier = P((CharPred(isIdentifierStart) ~ CharsWhile(isIdentifierPart, min = 0)).!)
    private val quotedString = P("\"" ~ CharsWhile(c ⇒ c != '"' && c != '\\').! ~ "\"")
    private val nodeId = identifier map NodeId.apply
    private val nodeIdDefinition = P(nodeId ~ h ~ ":" ~ w)
    private val javaClassName = P((identifier ~ ("." ~ identifier).rep).!)

    private val pathString = P(("/" ~ identifier ~ ("/" ~ identifier).rep).!)
    private def path[P <: TypedPath: TypedPath.Companion] =
      P(pathString map implicitly[TypedPath.Companion[P]].apply)

    private lazy val curlyScript: Parser[WorkflowScript] =
      P("{" ~ w ~ script ~ w ~ "}")

    private val agentJobPath =
      P(("job" ~ w ~ path[JobPath] ~ w ~ "at" ~ w ~ path[AgentPath])
        .map { case (j, a) ⇒ AgentJobPath(a, j) })

    private val jobStatement: Parser[WorkflowScript.Job] =
      P((nodeIdDefinition.? ~ agentJobPath)
        .map {
          case (Some(n), a) ⇒ n → a
          case (None, a) ⇒ NodeId(a.jobPath.withoutStartingSlash) → a
        }
        .map { case (n, a) ⇒ WorkflowScript.Job(n, a)})

    private val endStatement: Parser[WorkflowScript.End] =
      P(nodeIdDefinition ~ w ~ "end")
        .map { n ⇒ WorkflowScript.End(n) }

    private val nodeStatement: Parser[WorkflowScript.NodeStatement] =
      P(jobStatement | endStatement)

    private val orderSuffix: Parser[OrderId.Child] =
      P(quotedString map OrderId.Child.apply)

    private val forkBranch: Parser[(OrderId.Child, WorkflowScript)] =
      P(orderSuffix ~ w ~ curlyScript)

    private val forkStatement: Parser[WorkflowScript.ForkJoin] =
      P(("fork" ~ w ~ inParentheses(w ~ forkBranch ~ (comma ~ forkBranch).rep ~ w))
        .map { case (orderSuffix_, script_, more) ⇒
          WorkflowScript.ForkJoin(ListMap(orderSuffix_ → script_) ++ more)
        })

    private val ifErrorStatement: Parser[WorkflowScript.IfError] =
      P(("ifError" ~ w ~ nodeId)
        .map { n ⇒ WorkflowScript.IfError(n) })

    private val gotoStatement: Parser[WorkflowScript.Goto] =
      P(("goto" ~ w ~ nodeId)
        .map { n ⇒ WorkflowScript.Goto(n) })

    private val statement: Parser[WorkflowScript.Statement] =
      P(nodeStatement | forkStatement | ifErrorStatement | gotoStatement)

    private val script: Parser[WorkflowScript] =
      P(jobStatement ~ statementTerminator ~ (statement ~ statementTerminator).rep) map {
        case (head, tail) ⇒ WorkflowScript(Vector(head) ++ tail)
      }

    val whole = w ~ script ~ w ~ End

    private def keyValue[V](name: String, valueParser: Parser[V]): Parser[V] =
      P(name ~ h ~ "=" ~ w ~ valueParser)

    private def inParentheses[A](parser: Parser[A]): Parser[A] =
      P(h ~ "(" ~ w ~ parser ~ w ~ ")")

    private def array[A](parser: Parser[A]): Parser[collection.Seq[A]] =
      P("[" ~ w ~ parser ~ (comma ~ parser).rep ~ w ~ "]") map {
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
