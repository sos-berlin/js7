package com.sos.jobscheduler.data.workflow.position

import com.sos.jobscheduler.base.utils.ScalaUtils.reuseIfEqual
import com.sos.jobscheduler.data.workflow.WorkflowId
import io.circe.generic.JsonCodec
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class WorkflowPosition(workflowId: WorkflowId, position: Position)
{
  lazy val normalized = reuseIfEqual(this, workflowId /: position.normalized)

  override def toString = s"${workflowId.toSimpleString}:$position"
}

object WorkflowPosition
{
  // TODO Should be explicit
  implicit def apply(workflowId: WorkflowId): WorkflowPosition =
    WorkflowPosition(workflowId, Position(InstructionNr.First))

  //implicit val jsonEncoder: Encoder.AsArray[WorkflowPosition] =
  //  absolute => absolute.workflowId.asJson +: Position.jsonEncoder.encodeArray(absolute.position)
  //  //absolute => Json.fromString(absolute.workflowId.string) +: Position.jsonEncoder.encodeArray(absolute.position)
  //
  //implicit val jsonDecoder: Decoder[WorkflowPosition] =
  //  cursor =>
  //    for {
  //      jsons <- cursor.as[Seq[Json]]
  //      absolute <-
  //        if (jsons.isEmpty)
  //          Left(DecodingFailure("Empty JSON array as Position.WorkflowPosition?", cursor.history))
  //        else
  //          for {
  //            workflowId <- jsons(0).as[WorkflowId]
  //            p <- Json.fromValues(jsons.tail).as[Position]
  //          } yield WorkflowPosition(workflowId, p)
  //          //for {
  //          //  idString <- jsons(0).as[String]
  //          //  workflowId <- WorkflowId.checked(idString).toDecoderResult
  //          //  p <- Json.fromValues(jsons.tail).as[Position]
  //          //} yield WorkflowPosition(workflowId, p)
  //    } yield absolute
}
