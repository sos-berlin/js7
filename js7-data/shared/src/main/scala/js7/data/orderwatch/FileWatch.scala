package js7.data.orderwatch

import io.circe.Codec
import io.circe.generic.extras.Configuration.default.withDefaults
import java.util.regex.Pattern
import js7.base.circeutils.CirceUtils.deriveConfiguredCodec
import js7.base.circeutils.ScalaJsonCodecs._
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.base.utils.SimplePattern
import js7.data.agent.AgentId
import js7.data.item.ItemRevision
import js7.data.value.expression.Expression
import js7.data.workflow.WorkflowPath
import scala.concurrent.duration.{Duration, FiniteDuration}

final case class FileWatch(
  id: OrderWatchId,
  workflowPath: WorkflowPath,
  agentId: AgentId,
  directory: String,
  pattern: Option[SimplePattern] = None,
  orderIdExpression: Option[Expression] = None,
  delay: FiniteDuration = Duration.Zero,
  itemRevision: ItemRevision = ItemRevision.Initial)
extends OrderWatch
{
  protected type Self = FileWatch
  val companion = FileWatch

  override def equals(other: Any) =
    other match {
      case o: FileWatch =>
        id == o.id &&
        workflowPath == o.workflowPath &&
        agentId == o.agentId &&
        directory == o.directory &&
        pattern.map(_.pattern) == o.pattern.map(_.pattern)   // Pattern itself does not compare

      case _ => false
    }

  def withRevision(revision: ItemRevision) =
    copy(itemRevision = revision)
}

object FileWatch extends OrderWatch.Companion
{
  type Item = FileWatch
  type Id = OrderWatchId

  val cls = classOf[FileWatch]
  val idCompanion = OrderWatchId
  val FileArgumentName = "file"

  implicit val jsonCodec: Codec.AsObject[FileWatch] = {
    implicit val configuration = withDefaults
    deriveConfiguredCodec[FileWatch]
  }

  intelliJuseImport(FiniteDurationJsonEncoder)
}
