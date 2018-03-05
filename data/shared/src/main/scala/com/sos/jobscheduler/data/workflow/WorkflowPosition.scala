package com.sos.jobscheduler.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{ArrayEncoder, Decoder, DecodingFailure, Json}
import scala.collection.immutable.Seq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowPosition(workflowId: WorkflowId, position: Position) {

  override def toString = s"$workflowId/$position"
}

object WorkflowPosition {

  implicit def apply(workflowId: WorkflowId): WorkflowPosition =
    WorkflowPosition(workflowId, Position(InstructionNr.First))

  implicit val jsonEncoder: ArrayEncoder[WorkflowPosition] =
    absolute ⇒ absolute.workflowId.asJson +: Position.jsonEncoder.encodeArray(absolute.position)
    //absolute ⇒ Json.fromString(absolute.workflowId.string) +: Position.jsonEncoder.encodeArray(absolute.position)

  implicit val jsonDecoder: Decoder[WorkflowPosition] =
    cursor ⇒
      for {
        jsons ← cursor.as[Seq[Json]]
        absolute ←
          if (jsons.isEmpty)
            Left(DecodingFailure("Empty JSON array as Position.WorkflowPosition?", Nil))
          else
            for {
              workflowId ← jsons(0).as[WorkflowId]
              p ← Json.fromValues(jsons.tail).as[Position]
            } yield WorkflowPosition(workflowId, p)
            //for {
            //  idString ← jsons(0).as[String]
            //  workflowId ← WorkflowId.checked(idString).toDecoderResult
            //  p ← Json.fromValues(jsons.tail).as[Position]
            //} yield WorkflowPosition(workflowId, p)
      } yield absolute
}
