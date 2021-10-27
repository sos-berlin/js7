package js7.data.orderwatch

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.extras.JsonKey
import io.circe.generic.extras.semiauto.deriveConfiguredEncoder
import io.circe.{Codec, Decoder}
import java.util.regex.Pattern
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.time.ScalaTime._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.item.ItemRevision
import js7.data.orderwatch.FileWatch.defaultPattern
import js7.data.value.expression.Expression
import js7.data.workflow.WorkflowPath
import scala.concurrent.duration.FiniteDuration

final case class FileWatch(
  path: OrderWatchPath,
  workflowPath: WorkflowPath,
  agentPath: AgentPath,
  @JsonKey("directoryExpr") directory: Expression,
  pattern: Option[SimplePattern] = None,
  orderIdExpression: Option[Expression] = None,
  delay: FiniteDuration = ZeroDuration,
  itemRevision: Option[ItemRevision] = None)
extends OrderWatch
{
  type Self = FileWatch
  val companion = FileWatch

  def resolvedPattern: Pattern =
    pattern.fold(defaultPattern)(_.pattern)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)

  override val referencedAgentPaths = Set(agentPath)

  override val referencedWorkflowPaths = Set(workflowPath)
}

object FileWatch extends OrderWatch.Companion[FileWatch]
{
  val cls = classOf[FileWatch]

  type Key = OrderWatchPath
  val Key = OrderWatchPath

  val FileArgumentName = "file"
  private val defaultPattern = Pattern.compile("[^.].*")

  implicit val jsonCodec: Codec.AsObject[FileWatch] = {
    val decoder: Decoder[FileWatch] =
      c => for {
        path <- c.get[OrderWatchPath]("path")
        workflowPath <- c.get[WorkflowPath]("workflowPath")
        agentPath <- c.get[AgentPath]("agentPath")
        directoryExpr <- c.get[Expression]("directoryExpr")
          .orElse(
            c.get[String]("directory")/*COMPATIBLE with v2.0.1*/.map(Expression.StringConstant(_)))
        pattern <- c.get[Option[SimplePattern]]("pattern")
        orderIdExpression <- c.get[Option[Expression]]("orderIdExpression")
        delay <- c.getOrElse[FiniteDuration]("delay")(ZeroDuration)
        itemRevision <- c.get[Option[ItemRevision]]("itemRevision")
      } yield FileWatch(path, workflowPath, agentPath,
        directoryExpr, pattern, orderIdExpression, delay, itemRevision)

    implicit val configuration = withDefaults
    Codec.AsObject.from(
      decoder,
      deriveConfiguredEncoder[FileWatch])
  }

  intelliJuseImport(FiniteDurationJsonEncoder)
}
