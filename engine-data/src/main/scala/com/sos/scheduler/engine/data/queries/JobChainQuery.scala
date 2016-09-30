package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions._
import com.sos.scheduler.engine.base.serial.PathAndParameterSerializable
import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.queries.JobChainQuery._
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class JobChainQuery(
  pathQuery: PathQuery = PathQuery.All,
  isDistributed: Option[Boolean] = None) {

  def toPathAndParameters: (String, Map[String, String]) =
    (pathQuery.toUriPath, (isDistributed map { o ⇒ IsDistributedName → o.toString }).toMap)

  def matchesAll = pathQuery.matchesAll && isDistributed.isEmpty

  def matches(jobChain: QueryableJobChain) =
    pathQuery.matches(jobChain.path) &&
    (isDistributed forall { _ == jobChain.isDistributed })
}

object JobChainQuery {
  type Interface = JobChainQuery

  val All = JobChainQuery()
  val PathName = "path"
  val IsDistributedName = "isDistributed"

  implicit object jsonFormat extends RootJsonFormat[JobChainQuery] {
    def write(q: JobChainQuery) = JsObject((
      (q.pathQuery != PathQuery.All option (PathName → JsString(q.pathQuery.toUriPath))) ++
        (q.isDistributed map { o ⇒ IsDistributedName → JsBoolean(o) }))
      .toMap)

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      JobChainQuery(
        pathQuery = fields.get(PathName) match {
          case Some(path) ⇒ PathQuery[JobChainPath](path.asInstanceOf[JsString].value)
          case None ⇒ PathQuery.All
        },
        isDistributed = fields.get(IsDistributedName) map { _.asInstanceOf[JsBoolean].value })
    }
  }

  implicit object pathAndParameterSerializable extends PathAndParameterSerializable[JobChainQuery] {
    def toPathAndParameters(q: JobChainQuery) = q.toPathAndParameters

    def fromPathAndParameters(pathAndParameters: (String, Map[String, String])) = {
      val (path, parameters) = pathAndParameters
      JobChainQuery(
        pathQuery = PathQuery[JobChainPath](path),
        isDistributed = parameters.optionAs[Boolean](IsDistributedName))
    }
  }
}
