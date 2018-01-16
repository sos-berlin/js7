package com.sos.jobscheduler.data.workflow

import com.sos.jobscheduler.base.circeutils.CirceUtils.deriveCirceCodec
import com.sos.jobscheduler.base.utils.Collections.implicits.RichPairTraversable
import com.sos.jobscheduler.data.agent.AgentPath
import com.sos.jobscheduler.data.workflow.Instruction._
import com.sos.jobscheduler.data.workflow.Workflow._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, JsonObject, ObjectEncoder}
import scala.collection.immutable.{IndexedSeq, Seq}

/**
  * @author Joacim Zschimmer
  */
final case class Workflow private(labeledInstructions: IndexedSeq[Instruction.Labeled], source: Option[String]) {

  assert(isCorrectlyEnded(labeledInstructions), "Missing implicit end instruction")

  val instructions: IndexedSeq[Instruction] = labeledInstructions map (_.instruction)
  val labelToNumber: Map[Label, InstructionNr] =
    numberedInstructions.flatMap { case (nr, Labeled(labels, _)) ⇒ labels map (_ → nr) }
      .uniqueToMap(labels ⇒ throw new IllegalArgumentException(s"Duplicate labels in Workflow: ${labels mkString ","}"))

  labeledInstructions foreach {
    case _ @: (stmt @ Goto(label)) if !labelToNumber.contains(label) ⇒ throw new IllegalArgumentException(s"Missing label '$label' in Workflow: $stmt")
    case _ @: (stmt @ IfError(label)) if !labelToNumber.contains(label) ⇒ throw new IllegalArgumentException(s"Missing label '$label' in Workflow: $stmt")
    case _ ⇒
  }

  //def firstExecutableInstructionNr: InstructionNr =

  def lastNr: InstructionNr =
    instructions.length - 1

  lazy val flatten: Seq[(Position, Instruction.Labeled)] =
    toFlats(Nil)

  private def toFlats(parents: List[Position.Parent]): Seq[(Position, Instruction.Labeled)] =
    positionAndInstructions(parents)
      .collect {
        case pi @ (pos, _ @: ForkJoin(branches)) ⇒
          Vector(pi) ++
            (for {
              branch ← branches
              flats ← branch.workflow.toFlats(pos.parents ::: Position.Parent(pos.nr, branch.id) :: Nil)
            } yield flats)

        case o ⇒
          Vector(o)
      }.flatten

  private def positionAndInstructions(parents: List[Position.Parent]): Seq[(Position, Instruction.Labeled)] =
    for ((nr, s) ← numberedInstructions) yield Position(parents, nr) → s

  private def numberedInstructions: Seq[(InstructionNr, Instruction.Labeled)] =
    labeledInstructions.zipWithIndex.map {
      case (s, i) ⇒ InstructionNr(i) → s
    }

  def reduce: Workflow =
    Workflow(
      labeledInstructions.sliding(2).flatMap { // Peep-hole optimize
        case Seq(_ @: (jmp: JumpInstruction), Labeled(labels, _)) if labels contains jmp.to ⇒ Nil
        case Seq(_ @: IfError(errorTo), _ @: Goto(to)) if errorTo == to ⇒ Nil
        case Seq(a, _) ⇒ a :: Nil
        case Seq(_) ⇒ Nil  // Unused code in contrast to sliding's documentation?
      }.toVector ++
        labeledInstructions.lastOption)

  def isPartiallyExecutableOnAgent(agentPath: AgentPath): Boolean =
    labeledInstructions map (_.instruction) collect {
      case o: Job ⇒ o isExecutableOnAgent agentPath
      case o: ForkJoin ⇒ o isPartiallyExecutableOnAgent agentPath
    } contains true

  def isStartableOnAgent(address: Position, agentPath: AgentPath): Boolean =
    isStartableOnAgent(instruction(address), agentPath)

  private def isStartableOnAgent(instruction: Instruction, agentPath: AgentPath): Boolean =
    instruction match {
      case o: ForkJoin ⇒ o.isStartableOnAgent(agentPath)
      case o: Job ⇒ o.isExecutableOnAgent(agentPath)
      case _ ⇒ false
    }

  def isStartableOnAgent(agentPath: AgentPath): Boolean =
    instructions.headOption collect { case o: Job ⇒ o } exists (_ isExecutableOnAgent agentPath)

  //def isEndingOnAgent(agentPath: AgentPath): Boolean =
  //  labeledInstructions.reverse.dropWhile(_.isInstanceOf[End]).headOption collect { case o: Job ⇒ o } exists (_ isExecutableOnAgent agentPath)

  def isDefinedAt(position: Position): Boolean =
    position match {
      case Position(Nil, nr) ⇒ isDefinedAt(nr)
      case Position(Position.Parent(nr, childId) :: tail, tailNr) ⇒
        instruction(nr) match {
          case ForkJoin(branches) ⇒ branches.find(_.id == childId) exists (_.workflow isDefinedAt Position(tail, tailNr))
          case _ ⇒ false
        }
    }

  def isDefinedAt(nr: InstructionNr): Boolean =
    labeledInstructions.indices isDefinedAt nr.number

  def jobOption(address: Position): Option[Job] =
    Some(instruction(address)) collect { case o: Job ⇒ o }

  def nextInstructionNr(position: Position): InstructionNr =
    position.nr.increment

  def instruction(position: Position): Instruction =
    position match {
      case Position(Nil, nr) ⇒
        instruction(nr)

      case Position(Position.Parent(nr, childId) :: tail, tailNr) ⇒
        instruction(nr) match {
          case fj: ForkJoin ⇒
            fj.branches find (_.id == childId) map (_.workflow.instruction(Position(tail, tailNr))) getOrElse Gap
          case _ ⇒
            Gap
        }
    }

  def instruction(nr: InstructionNr): Instruction =
    instructions(nr.number)
}

object Workflow {
  private val empty = Workflow(Vector.empty)

  def apply(labeledInstructions: IndexedSeq[Instruction.Labeled], source: Option[String] = None): Workflow =
    new Workflow(
      labeledInstructions =
        if (isCorrectlyEnded(labeledInstructions))
          labeledInstructions
        else
          labeledInstructions :+ (() @: ImplicitEnd),
      source)

  def of(instructions: Instruction*): Workflow =
    if (instructions.isEmpty)
      empty
    else
      Workflow(instructions.map(() @: _).toVector)

  def apply(): Workflow = empty

  def isCorrectlyEnded(labeledInstructions: IndexedSeq[Labeled]): Boolean =
    labeledInstructions.nonEmpty &&
      (labeledInstructions.last.instruction.isInstanceOf[End] ||
       labeledInstructions.last.instruction.isInstanceOf[Goto])

  final case class Named(path: WorkflowPath, workflow: Workflow) {
    @deprecated
    def lastWorkflowPosition = path /: Position(workflow.lastNr)
  }
  object Named {
    implicit val jsonCodec = deriveCirceCodec[Named]
  }

  implicit val jsonEncoder: ObjectEncoder[Workflow] = {
    case Workflow(instructions, source) ⇒
      JsonObject.fromIterable(Array(
        "instructions" → instructions.asJson,
        "source" → source.asJson))
  }
  implicit val jsonDecoder: Decoder[Workflow] =
    cursor ⇒ for {
      instructions ← cursor.get[IndexedSeq[Labeled]]("instructions")
      source ← cursor.get[Option[String]]("source")
    } yield Workflow(instructions, source)
}
