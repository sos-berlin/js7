package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{deriveCirceCodec, listMapCodec}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichPairTraversable
import com.sos.jobscheduler.base.utils.DuplicateKeyException
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

  object Statement {
    private[WorkflowScript] implicit val statementJsonCodec = TypedJsonCodec[Statement](
      Subtype(deriveCirceCodec[Job]),
      Subtype(deriveCirceCodec[End]),
      //Subtype(circeCodec(ForkJoin.jsonEncoder, ForkJoin.jsonDecoder)),
      Subtype(ForkJoin.jsonCodec),
      Subtype(deriveCirceCodec[OnError]),
      Subtype(deriveCirceCodec[Goto]))
  }

  implicit val jsonCodec: CirceCodec[WorkflowScript] = deriveCirceCodec[WorkflowScript]

  sealed trait NodeStatement extends Statement {
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

    //private[WorkflowScript] def jsonEncoder: Encoder[ForkJoin] =
    //  forkJoin ⇒ Json.obj(
    //    "scripts" → Json.fromValues(
    //      for ((id, script) ← forkJoin.idToScript) yield
    //        Json.fromJsonObject(JsonObject.fromMap(ListMap("id" → id.asJson) ++ script.asJson.asObject.get.toMap))))
    //
    //private implicit val idAndScriptDecoder: Decoder[(WorkflowGraph.Id, WorkflowScript)] =
    //  cursor ⇒
    //    for {
    //      id ← cursor.downField("id").as[WorkflowGraph.Id]
    //      script ← cursor.as[WorkflowScript]
    //    } yield
    //      id → script
    //
    //private[WorkflowScript] def jsonDecoder: Decoder[ForkJoin] =
    //  cursor ⇒
    //    for {
    //      idAndScripts ← cursor.downField("scripts").as[Seq[(WorkflowGraph.Id, WorkflowScript)]]
    //    } yield ForkJoin(ListMap.empty ++ idAndScripts)
  }

  final case class OnError(to: NodeId) extends Statement {
    def nodes = Nil
  }

  final case class Goto(to: NodeId) extends Statement {
    def nodes = Nil
  }
}

