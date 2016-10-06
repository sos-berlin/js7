package com.sos.scheduler.engine.data.queries

import com.sos.scheduler.engine.base.convert.ConvertiblePartialFunctions.ImplicitConvertablePF
import com.sos.scheduler.engine.base.serial.PathAndParameterSerializable
import com.sos.scheduler.engine.data.job.JobPath
import com.sos.scheduler.engine.data.jobchain.NodeId
import com.sos.scheduler.engine.data.queries.CommaSeparated._
import com.sos.scheduler.engine.data.queries.JobChainNodeQuery._
import spray.json.DefaultJsonProtocol._
import spray.json._
import scala.language.implicitConversions

/**
  * @author Joacim Zschimmer
  */
final case class JobChainNodeQuery(
  jobChainQuery: JobChainQuery = JobChainQuery.All,
  nodeIds: Option[Set[NodeId]] = None,
  jobPaths: Option[Set[JobPath]] = None) {

  def toPathAndParameters: (String, Map[String, String]) = {
    val (path, parameters) = jobChainQuery.toPathAndParameters
    (path, parameters ++
      toNamedCommaSeparated(JobPathsName, jobPaths)(_.string) ++
      toNamedCommaSeparated(NodeIdsName, nodeIds)(_.string))
  }

  def matchesAll = jobChainQuery.matchesAll && matchesCompleteJobChains

  def matchesAllNonDistributed = matchesCompleteJobChains && jobChainQuery.matchesAllNonDistributed

  def matchesCompleteJobChains = nodeIds.isEmpty && jobPaths.isEmpty

  def matches(node: QueryableJobNode) =
    jobChainQuery.matches(node.jobChain) &&
      (nodeIds forall { _ contains node.nodeId }) &
      (jobPaths forall { _ contains node.jobPath })
}

object JobChainNodeQuery {
  val All = JobChainNodeQuery()

  val JobPathsName = "jobPaths"
  val NodeIdsName = "nodeIds"

  implicit def fromJobChainQuery(o: JobChainQuery): JobChainNodeQuery = JobChainNodeQuery(o)

  implicit def fromJobChainQuery(o: PathQuery): JobChainNodeQuery = JobChainNodeQuery(o)

  implicit object jsonFormat extends RootJsonFormat[JobChainNodeQuery] {
    def write(q: JobChainNodeQuery) = JsObject(
      q.jobChainQuery.toJson.asJsObject.fields ++
      (q.nodeIds map { o ⇒ NodeIdsName → o.toJson }) ++
      (q.jobPaths map { o ⇒ JobPathsName → o.toJson }))

    def read(json: JsValue) = {
      val fields = json.asJsObject.fields
      JobChainNodeQuery(
        jobChainQuery = json.convertTo[JobChainQuery],
        nodeIds = fields.get(NodeIdsName) map { _.convertTo[Set[NodeId]] },
        jobPaths = fields.get(JobPathsName) map { _.convertTo[Set[JobPath]] })
    }
  }

  implicit object pathAndParameterSerializable extends PathAndParameterSerializable[JobChainNodeQuery] {
    def toPathAndParameters(q: JobChainNodeQuery) = q.toPathAndParameters

    def fromPathAndParameters(pathAndParameters: (String, Map[String, String])) = {
      val (path, parameters) = pathAndParameters
      JobChainNodeQuery(
        jobChainQuery = PathAndParameterSerializable.fromPathAndParameters[JobChainQuery]((path, parameters)),
        nodeIds = parameters.optionAs(NodeIdsName)(commaSplittedAsSet(NodeId.apply)),
        jobPaths = parameters.optionAs(JobPathsName)(commaSplittedAsSet(JobPath.apply)))
    }
  }
}
