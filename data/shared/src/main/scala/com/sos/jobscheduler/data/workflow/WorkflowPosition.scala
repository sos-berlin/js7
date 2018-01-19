package com.sos.jobscheduler.data.workflow

import io.circe.syntax.EncoderOps
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import scala.collection.immutable.Seq
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class WorkflowPosition(workflowPath: WorkflowPath, position: Position) {

  override def toString = s"$workflowPath/$position"
}

object WorkflowPosition {

  implicit def apply(workflowPath: WorkflowPath): WorkflowPosition =
    WorkflowPosition(workflowPath, Position(InstructionNr.First))

  implicit val jsonEncoder: Encoder[WorkflowPosition] =
    absolute ⇒ Json.fromValues(absolute.workflowPath.asJson +: Position.toJsonSeq(absolute.position))

  implicit val jsonDecoder: Decoder[WorkflowPosition] =
    cursor ⇒
      for {
        jsons ← cursor.as[Seq[Json]]
        absolute ←
          if (jsons.isEmpty)
            Left(DecodingFailure("Empty JSON array as Position.WorkflowPosition?", Nil))
          else
            for {
              w ← jsons.head.as[WorkflowPath]
              p ← Json.fromValues(jsons.tail).as[Position]
            } yield WorkflowPosition(w, p)
      } yield absolute
}
