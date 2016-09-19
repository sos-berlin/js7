package com.sos.scheduler.engine.data.queries

/**
  * @author Joacim Zschimmer
  */
trait JobChainQuery {
  def jobChainPathQuery: PathQuery
  def isDistributed: Option[Boolean]

  def withJobChainPathQuery(q: PathQuery): JobChainQuery

  final def matchesAllJobChains = jobChainPathQuery.matchesAll && isDistributed.isEmpty

  final def matchesJobChain(jobChain: QueryableJobChain) =
    jobChainPathQuery.matches(jobChain.path) &&
    (isDistributed forall { _ == jobChain.isDistributed })
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
}
