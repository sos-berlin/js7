package js7.data_for_java.orderwatch

import io.vavr.control.{Either => VEither}
import java.nio.file.{Path, Paths}
import java.util.Optional
import java.util.regex.Pattern
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.data.agent.AgentId
import js7.data.orderwatch.{FileWatch, OrderWatchId}
import js7.data.workflow.WorkflowPath
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JSimpleItem
import scala.jdk.OptionConverters._

final case class JFileWatch(asScala: FileWatch)
extends JJsonable[JFileWatch] with JSimpleItem
{
  protected type AsScala = FileWatch
  protected def companion = JFileWatch

  @Nonnull
  def id: OrderWatchId =
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

object JFileWatch extends JJsonable.Companion[JFileWatch]
{
  @Nonnull
  def of(
    @Nonnull id: OrderWatchId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull agentId: AgentId,
    @Nonnull directory: Path)
  : JFileWatch =
    JFileWatch(FileWatch(id, workflowPath, agentId, directory.toString))

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JFileWatch] =
    super.fromJson(jsonString)

  protected def jsonEncoder = FileWatch.jsonCodec
  protected def jsonDecoder = FileWatch.jsonCodec
}
