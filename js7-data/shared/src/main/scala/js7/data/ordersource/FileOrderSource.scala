package js7.data.ordersource

import io.circe.generic.extras.Configuration.default.withDefaults
import io.circe.generic.semiauto.deriveCodec
import java.util.regex.Pattern
import js7.base.circeutils.CirceObjectCodec
import js7.base.circeutils.StandardJsonCodecs.PatternJsonCodec
import js7.base.problem.Checked
import js7.base.utils.IntelliJUtils.intelliJuseImport
import js7.data.agent.AgentId
import js7.data.item.ItemRevision
import js7.data.order.OrderId
import js7.data.workflow.WorkflowPath

final case class FileOrderSource(
  id: OrderSourceId,
  workflowPath: WorkflowPath,
  agentId: AgentId,
  directory: String,
  pattern: Option[Pattern] = None,
  itemRevision: ItemRevision = ItemRevision.Initial)
extends OrderSource
{
  protected type Self = FileOrderSource
  val companion = FileOrderSource

  override def equals(other: Any) =
    other match {
      case o: FileOrderSource =>
        id == o.id &&
        workflowPath == o.workflowPath &&
        agentId == o.agentId &&
        directory == o.directory &&
        pattern.map(_.pattern) == o.pattern.map(_.pattern)   // Pattern does not compare

      case _ => false
    }

  def withRevision(revision: ItemRevision) =
    copy(itemRevision = revision)

  def generateOrderId(sourceOrderName: SourceOrderName): Checked[OrderId] =
    OrderId.checked(s"FileOrderSource:${id.string}:${sourceOrderName.string}")  // TODO Syntax?
}

object FileOrderSource extends OrderSource.Companion
{
  type Item = FileOrderSource
  type Id = OrderSourceId

  val cls = classOf[FileOrderSource]
  val idCompanion = OrderSourceId
  val FileArgumentName = "file"

  implicit val jsonCodec: CirceObjectCodec[FileOrderSource] = {
    implicit val configuration = withDefaults
    deriveCodec[FileOrderSource]
  }

  intelliJuseImport(PatternJsonCodec)
}
