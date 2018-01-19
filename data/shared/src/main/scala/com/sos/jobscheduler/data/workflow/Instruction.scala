package com.sos.jobscheduler.data.workflow

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.sos.jobscheduler.base.circeutils.CirceCodec
import com.sos.jobscheduler.base.circeutils.CirceUtils.{deriveCirceCodec, objectCodec}
import com.sos.jobscheduler.base.circeutils.typed.{Subtype, TypedJsonCodec}
import com.sos.jobscheduler.base.utils.Collections.implicits.RichIndexedSeq
import com.sos.jobscheduler.base.utils.Strings.TruncatedString
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.job.ReturnCode
import com.sos.jobscheduler.data.order.{Order, OrderId, Outcome}
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
  def toShortString = toString truncateWithEllipsis 40

  final def @:(labels: Seq[Label]) = Labeled(labels, this)
  final def @:(label: Label) = Labeled(label :: Nil, this)
  final def @:(label: String) = Labeled(Label(label) :: Nil, this)
  final def @:(unit: Unit) = Labeled(Nil, this)
}

object Instruction {
  val @: = Labeled

  /** Only for tests and application. */
  object simplify {
    implicit def fromInstruction(instruction: Instruction): Labeled =
      Labeled(Nil, instruction)
  }

  final case class Labeled(labels: Seq[Label], instruction: Instruction) {
    def toShortString = s"$labelsString ${instruction.toShortString}"

    override def toString = s"$labelsString $instruction"

    def labelsString = labels.map(o ⇒ s"$o: ").mkString
  }
  object Labeled {
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

    def workflowOption(branchId: Position.BranchId.Named): Option[Workflow] =
      branches collectFirst { case fj: ForkJoin.Branch if fj.id == branchId ⇒ fj.workflow }

    override def toShortString = s"ForkJoin(${branches.map(_.id).mkString(",")})"
  }
  object ForkJoin {
    implicit lazy val jsonCodec: CirceCodec[ForkJoin] = deriveCirceCodec[ForkJoin]

    def of(idAndWorkflows: (String, Workflow)*) =
      new ForkJoin(idAndWorkflows.map { case (id, workflow) ⇒ Branch(id, workflow) } .toVector)

    private def validateBranch(branch: Branch): Validated[RuntimeException, Branch] =
      if (branch.workflow.instructions exists (o ⇒ o.isInstanceOf[Goto]  || o.isInstanceOf[IfErrorGoto]))
        Invalid(new IllegalArgumentException(s"Fork/Join branch '${branch.id}' cannot contain a jump instruction like 'goto' or 'ifError'"))
      else
        Valid(branch)

    @JsonCodec
    final case class Branch(id: Position.BranchId.Named, workflow: Workflow)
    object Branch {
      implicit def fromPair(pair: (Position.BranchId.Named, Workflow)) = new Branch(pair._1, pair._2)
    }
  }

  @JsonCodec
  final case class IfReturnCode(returnCodes: Seq[ReturnCode], workflows: IndexedSeq[Workflow]) extends Instruction {
    require(workflows.size >= 1 && workflows.size <= 2)

    def nextPosition(order: Order[Order.State], idToOrder: PartialFunction[OrderId, Order[Order.State]]): Option[Position] = {
      assert(order == idToOrder(order.id).withPosition(order.position))
      Some(order.outcome) collect {
        case Outcome.Good(okay) ⇒
          val index = if (returnCodes contains ReturnCode(if (okay) 0 else 1)) 0 else 1
          if (workflows.indices contains index)
            Position(Position.Parent(order.position.nr, index) :: Nil, 0)
          else
            order.position.increment  // Skip statement
      }
    }

    override def toString = s"IfReturnCode ${returnCodes map (_.number) mkString ", "} then $thenWorkflow" +
      (elseWorkflow map (w ⇒ s" else $w") getOrElse "")

    private def thenWorkflow: Workflow =
      workflows(0)

    private def elseWorkflow: Option[Workflow] =
      workflows.get(1)

    def workflowOption(branchId: Position.BranchId.Indexed): Option[Workflow] =
      workflows.get(branchId.number)
  }

  sealed trait JumpInstruction extends Instruction {
    def to: Label
  }

  final case class IfErrorGoto(to: Label) extends JumpInstruction {
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
    Subtype(deriveCirceCodec[IfReturnCode]),
    Subtype(deriveCirceCodec[IfErrorGoto]),
    Subtype(deriveCirceCodec[Goto]),
    Subtype(objectCodec(Gap)))
}
