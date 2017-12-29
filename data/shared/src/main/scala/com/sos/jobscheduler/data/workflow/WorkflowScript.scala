package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{deriveCirceCodec, listMapCodec}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.generic.IsString
import com.sos.jobscheduler.base.utils.Collections.implicits.RichPairTraversable
import com.sos.jobscheduler.base.utils.DuplicateKeyException
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.WorkflowScript._
import scala.collection.immutable.{ListMap, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowScript(statements: Seq[Statement]) {

  val head: Job = statements.headOption match {
    case None ⇒ throw new IllegalArgumentException("WorkflowScript must not be empty")
    case Some(job: Job) ⇒ job
    case Some(_) ⇒ throw new IllegalArgumentException("First statement of a graph must be a job")
  }

  val idToNodeStatement = statements.collect {
    case o: NodeStatement ⇒ o.node.id → o
  }.uniqueToMap(duplicates ⇒ throw new DuplicateKeyException(s"WorkflowScript cannot have duplicate NodeIds: ${duplicates.mkString(",")}"))

  lazy val flatten: Seq[FlatStatement] =
    toFlats(FlatStatement.Nesting.empty)

  private def toFlats(nesting: FlatStatement.Nesting): Seq[FlatStatement] =
    statements.collect {
      case o: SimpleStatement ⇒
        Vector(FlatStatement.Simple(nesting, o))
      case ForkJoin(idToScript) ⇒
        Vector(FlatStatement.Fork(nesting)) ++ (
        for {
          (id, script) ← idToScript
          flats ← script.toFlats(nesting / id)
        } yield
          flats)
    }.flatten

  def startNode = head.node
  def nodes = statements.flatMap(_.nodes)

  def reduce: WorkflowScript =
    WorkflowScript(
      statements.sliding(2).flatMap {
        case Seq(Goto(to), b: NodeStatement) if to == b.node.id ⇒ Nil
        case Seq(OnError(errorTo), Goto(to)) if errorTo == to ⇒ Nil
        //case Seq(Goto(to), _) ⇒ a :: Nil
        case Seq(a, _) ⇒ a :: Nil
        case Seq(_) ⇒ Nil  // Unused code in contrast to sliding's documentation?
      }.toVector ++
        statements.lastOption)
}

object WorkflowScript {
  final case class Named(path: WorkflowPath, script: WorkflowScript)
  object Named {
    implicit val jsonCodec = deriveCirceCodec[Named]
  }

  sealed trait Statement {
    def nodes: Seq[WorkflowGraph.Node]
  }

  sealed trait SimpleStatement extends Statement

  object Statement {
    private[WorkflowScript] implicit val statementJsonCodec = TypedJsonCodec[Statement](
      Subtype(deriveCirceCodec[Job]),
      Subtype(deriveCirceCodec[End]),
      Subtype(ForkJoin.jsonCodec),
      Subtype(deriveCirceCodec[OnError]),
      Subtype(deriveCirceCodec[Goto]))
  }

  implicit val jsonCodec: CirceCodec[WorkflowScript] = deriveCirceCodec[WorkflowScript]

  sealed trait NodeStatement extends SimpleStatement {
    def node: WorkflowGraph.Node

    final def nodes = node :: Nil
  }

  final case class Job(nodeId: NodeId, job: AgentJobPath) extends NodeStatement {
    val node = WorkflowGraph.JobNode(nodeId, job)
  }

  final case class End(nodeId: NodeId) extends NodeStatement {
    val node = WorkflowGraph.EndNode(nodeId)
  }

  final case class ForkJoin(idToScript: ListMap[WorkflowGraph.Id, WorkflowScript])
  extends Statement {
    def nodes = idToScript.values.flatMap(_.nodes).toVector
  }
  object ForkJoin {
    implicit val myListMapCodec = listMapCodec[WorkflowGraph.Id, WorkflowScript](keyName = "id", valueName = "script")
    implicit lazy val jsonCodec: CirceCodec[ForkJoin] = deriveCirceCodec[ForkJoin]
  }

  final case class OnError(to: NodeId) extends SimpleStatement {
    def nodes = Nil
  }

  final case class Goto(to: NodeId) extends SimpleStatement {
    def nodes = Nil
  }

  sealed trait FlatStatement {
    def nesting: FlatStatement.Nesting
    //def nodeId: Option[NodeId]
  }
  object FlatStatement {
    final case class Nesting(string: String) extends IsString {
      def /(id: WorkflowGraph.Id): Nesting =
        string match {
          case "" ⇒ Nesting(id.string)
          case _ ⇒ Nesting(string + OrderId.ChildSeparator + id.string)
        }
    }
    object Nesting {
      val empty = Nesting("")
    }

    final case class Simple(nesting: Nesting, statement: SimpleStatement) extends FlatStatement {
      //def nodeId = statement match {
      //  case o: NodeStatement ⇒ Some(o.node.id)
      //  case _ ⇒ None
      //}
    }

    final case class Fork(nesting: Nesting) extends FlatStatement {
      //def nodeId = None
    }
  }
}

