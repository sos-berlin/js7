package js7.data_for_java.ordersource

import io.vavr.control.{Either => VEither}
import java.nio.file.{Path, Paths}
import java.util.Optional
import java.util.regex.Pattern
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.agent.AgentId
import js7.data.ordersource.{FileOrderSource, OrderSourceId}
import js7.data.workflow.WorkflowPath
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JSimpleItem
import scala.jdk.OptionConverters._

final case class JFileOrderSource(asScala: FileOrderSource)
extends JJsonable[JFileOrderSource] with JSimpleItem
{
  protected type AsScala = FileOrderSource
  protected def companion = JFileOrderSource

  @Nonnull
  def id: OrderSourceId =
    asScala.id

  @Nonnull
  def workflowPath: WorkflowPath =
    asScala.workflowPath

  @Nonnull
  def agentId: AgentId =
    asScala.agentId

  @Nonnull
  lazy val directory: Path =
    Paths.get(asScala.directory)

  @Nonnull
  def pattern: Optional[Pattern] =
    asScala.pattern.toJava
}

object JFileOrderSource extends JJsonable.Companion[JFileOrderSource]
{
  @Nonnull
  def of(
    @Nonnull id: OrderSourceId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull agentId: AgentId,
    @Nonnull directory: Path)
  : JFileOrderSource =
    JFileOrderSource(FileOrderSource(id, workflowPath, agentId, directory.toString))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JFileOrderSource] =
    super.fromJson(jsonString)

  protected def jsonEncoder = FileOrderSource.jsonCodec
  protected def jsonDecoder = FileOrderSource.jsonCodec
}
