package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.data.jobchain.QueryableJobChain
import spray.json.DefaultJsonProtocol._

/**
  * @author Joacim Zschimmer
  */
trait JobChainQuery {
  def jobChainPathQuery: PathQuery
  def isDistributed: Option[Boolean]

  final def matchesAllJobChains = jobChainPathQuery.matchesAll && isDistributed.isEmpty

  final def matchesJobChain(jobChain: QueryableJobChain) =
    jobChainPathQuery.matches(jobChain.path) &&
    (isDistributed forall { _ == jobChain.isDistributed })

  def withJobChainPathQuery(q: PathQuery): JobChainQuery
  def withIsDistributed(o: Boolean): JobChainQuery
}

object JobChainQuery {

  val All: JobChainQuery = Standard()

  final case class Standard(
    jobChainPathQuery: PathQuery = PathQuery.All,
    isDistributed: Option[Boolean] = None)
  extends JobChainQuery {

    def withJobChainPathQuery(q: PathQuery) = copy(jobChainPathQuery = q)
    def withIsDistributed(o: Boolean) = copy(isDistributed = Some(o))
  }

  object Standard {
    implicit val MyJsonFormat = jsonFormat2(apply)
  }
}
