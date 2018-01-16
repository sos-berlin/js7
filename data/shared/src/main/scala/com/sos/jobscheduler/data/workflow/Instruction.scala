package com.sos.jobscheduler.data.workflow

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{deriveCirceCodec, listMapCodec, objectCodec}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.order.OrderId
import com.sos.jobscheduler.data.workflow.Instruction.Labeled
import io.circe.generic.JsonCodec
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, Json}
import scala.collection.immutable.{IndexedSeq, Seq}
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
sealed trait Instruction {
  final def @:(labels: Seq[Label]) = Labeled(labels, this)
  final def @:(label: Label) = Labeled(label :: Nil, this)
  final def @:(label: String) = Labeled(Label(label) :: Nil, this)
  final def @:(unit: Unit) = Labeled(Nil, this)
}

object Instruction {
  val @: = Labeled

  final case class Labeled(labels: Seq[Label], instruction: Instruction)
  object Labeled {
    implicit def fromInstruction(instruction: Instruction): Labeled =
      Labeled(Nil, instruction)

    implicit val jsonEncoder: Encoder[Labeled] = {
      case Labeled(Seq(), instruction) ⇒
        instruction.asJson
      case Labeled(labels, instruction) ⇒
        Json.fromJsonObject(("labels" → labels.asJson) +: instruction.asJson.asObject.get)
    }
    implicit val jsonDecoder: Decoder[Labeled] =
      cursor ⇒ for {
        instruction ← cursor.as[Instruction]
        labels ← cursor.get[Json]("labels") match {
          case Right(json) ⇒ json.as[Seq[Label]]
          case Left(_) ⇒ Right(Nil)
        }
      } yield Labeled(labels, instruction)
  }

  final case class Job(job: AgentJobPath) extends Instruction {
    def agentPath = job.agentPath

    def jobPath = job.jobPath

    def isExecutableOnAgent(agentPath: AgentPath): Boolean =
      job.agentPath == agentPath

    override def toString = s"job ${jobPath.string} on ${agentPath.string}"
  }

  sealed trait End extends Instruction
  case object ExplicitEnd extends End {
    override def toString = "end"
  }
  case object ImplicitEnd extends End {
    override def toString = "end/*implicit*/"
  }

  sealed trait Gap extends Instruction
  /** reduceForAgent uses Gap for all instructions not executable on the requested Agent. */
  case object Gap extends Gap {
    override def toString = "gap"
  }

  final case class ForkJoin(branches: IndexedSeq[ForkJoin.Branch])
  extends Instruction {
    for (idAndScript ← branches) ForkJoin.validateBranch(idAndScript).valueOr(throw _)

    def isPartiallyExecutableOnAgent(agentPath: AgentPath): Boolean =
      branches exists (_.workflow isPartiallyExecutableOnAgent agentPath)

    def isStartableOnAgent(agentPath: AgentPath): Boolean =
      // Any Agent or the master can fork. The current Agent is okay.
      branches exists (_.workflow isStartableOnAgent agentPath)

    //def isJoinableOnAgent(agentPath: AgentPath): Boolean =
    //  // If branches end on multiple Agents, only the Master can join the Orders
    //  branches.values forall (_ isEndingOnAgent agentPath)
  }
  object ForkJoin {
    implicit val myListMapCodec = listMapCodec[OrderId.ChildId, Workflow](keyName = "id", valueName = "workflow")
    implicit lazy val jsonCodec: CirceCodec[ForkJoin] = deriveCirceCodec[ForkJoin]

    def of(idAndWorkflows: (OrderId.ChildId, Workflow)*) =
      new ForkJoin(idAndWorkflows.map { case (id, workflow) ⇒ Branch(id, workflow) } .toVector)

    private def validateBranch(branch: Branch): Validated[RuntimeException, Branch] = {
      if (branch.workflow.instructions exists (o ⇒ o.isInstanceOf[Goto]  || o.isInstanceOf[IfError]))
        Invalid(new IllegalArgumentException(s"Fork/Join branch '${branch.id}' cannot contain a jump instruction like 'goto' or 'ifError'"))
      else
        Valid(branch)
    }

    @JsonCodec
    final case class Branch(id: OrderId.ChildId, workflow: Workflow)
    object Branch {
      implicit def fromPair(pair: (OrderId.ChildId, Workflow)) = new Branch(pair._1, pair._2)
    }
  }

  sealed trait JumpInstruction extends Instruction {
    def to: Label
  }

  final case class IfError(to: Label) extends JumpInstruction {
    def nodes = Nil

    override def toString = s"ifError $to"
  }

  final case class Goto(to: Label) extends JumpInstruction {
    def nodes = Nil

    override def toString = s"goto $to"
  }
  private[workflow] implicit val instructionJsonCodec: TypedJsonCodec[Instruction] = TypedJsonCodec[Instruction](
    Subtype(deriveCirceCodec[Job]),
    Subtype.named(objectCodec(ExplicitEnd), "End"),
    Subtype(objectCodec(ImplicitEnd)),  // Serialized for easier external use of Workflow
    Subtype(ForkJoin.jsonCodec),
    Subtype(deriveCirceCodec[IfError]),
    Subtype(deriveCirceCodec[Goto]),
    Subtype(objectCodec(Gap)))
}
