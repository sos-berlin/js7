package js7.executor.forjava.internal

import js7.data.value.Value
import js7.data_for_java.common.JavaWrapper
import js7.executor.internal.InternalJob.Result
import scala.jdk.CollectionConverters._

final case class JOrderResult(asScala: Result)
extends JavaWrapper
{
  type AsScala = Result
}

object JOrderResult
{
  def of(namedValues: java.util.Map[String, Value]): JOrderResult =
    JOrderResult(Result(namedValues.asScala.toMap))
}
