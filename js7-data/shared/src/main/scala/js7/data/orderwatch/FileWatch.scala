package js7.data.orderwatch

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import java.util.regex.Pattern
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.item.ItemRevision
import js7.data.orderwatch.FileWatch.defaultPattern
import js7.data.value.expression.Expression
import js7.data.workflow.WorkflowPath
import scala.concurrent.duration.{Duration, FiniteDuration}

final case class FileWatch(
  path: OrderWatchPath,
  workflowPath: WorkflowPath,
  agentPath: AgentPath,
  directory: String,
  pattern: Option[SimplePattern] = None,
  orderIdExpression: Option[Expression] = None,
  delay: FiniteDuration = Duration.Zero,
  itemRevision: Option[ItemRevision] = None)
extends OrderWatch
{
  type Self = FileWatch
  val companion = FileWatch

  def resolvedPattern: Pattern =
    pattern.fold(defaultPattern)(_.pattern)

  def withRevision(revision: Option[ItemRevision]) =
    copy(itemRevision = revision)
}

object FileWatch extends OrderWatch.Companion[FileWatch]
{
  val cls = classOf[FileWatch]

  type Key = OrderWatchPath
  val Key = OrderWatchPath

  val FileArgumentName = "file"
  private val defaultPattern = Pattern.compile("[^.].*")

  implicit val jsonCodec: Codec.AsObject[FileWatch] = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[FileWatch]
  }

  intelliJuseImport(FiniteDurationJsonEncoder)
}
