package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions._
import com.sos.scheduler.engine.base.serial.PathAndParameterSerializable
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import scala.language.implicitConversions
import spray.json._

/**
  * @author Joacim Zschimmer
  */
final case class JobChainQuery(
  pathQuery: PathQuery = PathQuery.All,
  isDistributed: Option[Boolean] = None) {

  def toPathAndParameters: (String, Map[String, String]) =
    JobChainQuery.pathAndParameterSerializable.toPathAndParameters(this)

  def matchesAll: Boolean =
    pathQuery.matchesAll && isDistributed.isEmpty

  def matchesAllNonDistributed: Boolean =
    pathQuery.matchesAll && isDistributed != Some(true)

  def matches(jobChain: QueryableJobChain): Boolean =
    pathQuery.matches(jobChain.path) &&
    (isDistributed forall { _ == jobChain.isDistributed })
}

object JobChainQuery {
  val All = JobChainQuery()
  val IsDistributedName = "isDistributed"
  private implicit val pathJsonFormat = PathQuery.jsonFormat[JobChainPath]

  implicit def fromPathQuery(o: PathQuery): JobChainQuery = JobChainQuery(o)

  implicit def fromJobChainPath(o: JobChainPath): JobChainQuery = JobChainQuery(o)

  implicit object jsonFormat extends RootJsonFormat[JobChainQuery] {
    def write(q: JobChainQuery) =
      JsObject(
        q.pathQuery.toJson.asJsObject.fields ++
        (q.isDistributed map { o ⇒ IsDistributedName → JsBoolean(o) })
      .toMap)

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      JobChainQuery(
        pathQuery = json.convertTo[PathQuery],
        isDistributed = fields.get(IsDistributedName) map { _.asInstanceOf[JsBoolean].value })
    }
  }

  implicit object pathAndParameterSerializable extends PathAndParameterSerializable[JobChainQuery] {
    private implicit val _x = PathQuery.pathAndParameterSerializable[JobChainPath]

    def toPathAndParameters(q: JobChainQuery): (String, Map[String, String]) = {
      val (path, parameters) = q.pathQuery.toPathAndParameters[JobChainPath]
      (path, parameters ++ (q.isDistributed map { o ⇒ IsDistributedName → o.toString }))
    }

    def fromPathAndParameters(pathAndParameters: (String, Map[String, String])) = {
      val (path, parameters) = pathAndParameters
      JobChainQuery(
        pathQuery = PathQuery[JobChainPath](path),
        isDistributed = parameters.optionAs[Boolean](IsDistributedName))
    }
  }
}
