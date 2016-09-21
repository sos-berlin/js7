package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.utils.ScalazStyle.OptionRichBoolean
import com.sos.scheduler.engine.data.jobchain.JobChainPath
import com.sos.scheduler.engine.data.queries.JobChainQuery.IsDistributedName
import spray.json._

/**
  * @author Joacim Zschimmer
  */
trait JobChainQuery {
  def jobChainPathQuery: PathQuery
  def isDistributed: Option[Boolean]

  def toUriPathAndParameters: (String, Map[String, String]) =
    (jobChainPathQuery.toUriPath, (isDistributed map { o ⇒ IsDistributedName → o.toString }).toMap)

  def withJobChainPathQuery(q: PathQuery): JobChainQuery

  final def matchesAllJobChains = jobChainPathQuery.matchesAll && isDistributed.isEmpty

  final def matchesJobChain(jobChain: QueryableJobChain) =
    jobChainPathQuery.matches(jobChain.path) &&
    (isDistributed forall { _ == jobChain.isDistributed })
}

object JobChainQuery {

  val All: JobChainQuery = Standard()
  private val PathName = "path"
  val IsDistributedName = "isDistributed"

  def apply(jobChainPathQuery: PathQuery = PathQuery.All, isDistributed: Option[Boolean] = None): JobChainQuery =
    Standard(jobChainPathQuery, isDistributed)

  final case class Standard(
    jobChainPathQuery: PathQuery = PathQuery.All,
    isDistributed: Option[Boolean] = None)
  extends JobChainQuery {

    def withJobChainPathQuery(q: PathQuery) = copy(jobChainPathQuery = q)
    def withIsDistributed(o: Boolean) = copy(isDistributed = Some(o))
  }

  implicit object jsonFormat extends RootJsonFormat[JobChainQuery] {
    def write(q: JobChainQuery) = JsObject((
      (q.jobChainPathQuery != PathQuery.All option (PathName → JsString(q.jobChainPathQuery.toUriPath))) ++
        (q.isDistributed map { o ⇒ IsDistributedName → JsBoolean(o) }))
      .toMap)

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      JobChainQuery.Standard(
        jobChainPathQuery = fields.get(PathName) match {
          case Some(path) ⇒ PathQuery[JobChainPath](path.asInstanceOf[JsString].value)
          case None ⇒ PathQuery.All
        },
        isDistributed = fields.get(IsDistributedName) map { _.asInstanceOf[JsBoolean].value })
    }
  }
}
