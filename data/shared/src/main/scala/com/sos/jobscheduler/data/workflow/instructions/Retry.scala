package com.sos.jobscheduler.data.workflow.instructions

import com.sos.jobscheduler.base.circeutils.ScalaJsonCodecs._
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.data.workflow.Instruction
import io.circe.generic.JsonCodec
import io.circe.generic.extras.defaults.defaultGenericConfiguration

/**
  * @author Joacim Zschimmer
  */
@JsonCodec
final case class Retry()
  //delays: IndexedSeq[FiniteDuration] = IndexedSeq.empty)
extends Instruction
{
  override def toString = {
    //val delay = delays.size match {
    //  case 0 => ""
    //  case 1 => ", delay=" + delays.head.toString     // TODO Syntax für Dauer
    //  case _ => ", delay=" + delays.map(_.toString).mkString("[", ", ", "]")
    //}
    //s"retry(limit=$limit$delay) $block"
    "retry"
  }
}

object Retry
{
  intelliJuseImport((defaultGenericConfiguration, FiniteDurationJsonEncoder, FiniteDurationJsonDecoder))
}
