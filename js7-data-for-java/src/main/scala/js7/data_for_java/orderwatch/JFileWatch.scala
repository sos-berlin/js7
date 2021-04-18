package js7.data_for_java.orderwatch

import cats.instances.option._
import cats.syntax.traverse._
import io.vavr.control.{Either => VEither}
import java.nio.file.{Path, Paths}
import java.util.Optional
import java.util.regex.Pattern
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.SimplePattern
import js7.data.agent.AgentId
import js7.data.orderwatch.{FileWatch, OrderWatchId}
import js7.data.value.expression.ExpressionParser
import js7.data.workflow.WorkflowPath
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import js7.data_for_java.vavr.VavrConverters.RichVavrOption
import scala.jdk.OptionConverters._

final case class JFileWatch(asScala: FileWatch)
extends JJsonable[JFileWatch] with JUnsignedSimpleItem
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
    asScala.pattern.map(_.pattern).toJava
}

object JFileWatch extends JJsonable.Companion[JFileWatch]
{
  @Nonnull
  def checked(
    @Nonnull id: OrderWatchId,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull agentId: AgentId,
    @Nonnull directory: Path,
    @Nonnull pattern: Optional[String],
    @Nonnull orderIdExpression: Optional[String],
    @Nonnull delay: java.time.Duration)
  : VEither[Problem, JFileWatch] =
    ( for {
        pattern <- pattern.toScala.traverse(SimplePattern.checked(_))
        orderIdExpression <- orderIdExpression.toScala.traverse(ExpressionParser.parse(_))
      } yield
        JFileWatch(FileWatch(
            id, workflowPath, agentId, directory.toString,
            pattern,
            orderIdExpression,
            delay.toFiniteDuration))
    ).toVavr


  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JFileWatch] =
    super.fromJson(jsonString)

  protected def jsonEncoder = FileWatch.jsonCodec
  protected def jsonDecoder = FileWatch.jsonCodec
}
