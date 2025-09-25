package js7.data_for_java.controller

import java.util.Optional
import js7.base.problem.Problem
import js7.data.controller.ControllerCommand
import js7.data_for_java.vavr.Standards.VEither
import js7.data_for_java.vavr.VavrConverters.RichVavrOption
import scala.jdk.CollectionConverters.SeqHasAsJava

object JControllerCommandResponse:

  @throws[ClassCastException]("if not a ControllerCommand.Batch.Response")
  def fromBatch(response: ControllerCommand.Response): Optional[BatchResponse] =
    response match
      case response: ControllerCommand.Batch.Response => Optional.of(BatchResponse(response))
      case _ => Optional.empty()


  final class BatchResponse private[JControllerCommandResponse](
    response: ControllerCommand.Batch.Response):

    def responses: java.util.List[VEither[Problem, ControllerCommand.Response]] =
      response.responses.map(_.toVavr).asJava

    override def toString =
      response.toString
