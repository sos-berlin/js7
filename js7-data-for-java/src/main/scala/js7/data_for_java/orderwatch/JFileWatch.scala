package js7.data_for_java.orderwatch

import cats.instances.option.*
import cats.syntax.traverse.*
import io.vavr.control.Either as VEither
import java.util.Optional
import java.util.regex.Pattern
import javax.annotation.Nonnull
import js7.base.problem.Problem
import js7.base.time.JavaTimeConverters.AsScalaDuration
import js7.base.utils.SimplePattern
import js7.data.agent.AgentPath
import js7.data.item.ItemRevision
import js7.data.orderwatch.{FileWatch, OrderWatchPath}
import js7.data.value.expression.ExpressionParser.parseExpression
import js7.data.workflow.WorkflowPath
import js7.data_for_java.common.JJsonable
import js7.data_for_java.item.JUnsignedSimpleItem
import js7.data_for_java.value.JExpression
import js7.data_for_java.vavr.VavrConverters.RichVavrOption
import scala.jdk.OptionConverters.*

final case class JFileWatch(asScala: FileWatch)
extends JJsonable[JFileWatch] with JUnsignedSimpleItem:
  type AsScala = FileWatch
  protected def companion = JFileWatch

  @Nonnull
  def path: OrderWatchPath =
    asScala.path

  @Nonnull
  def workflowPath: WorkflowPath =
    asScala.workflowPath

  @Nonnull
  def agentPath: AgentPath =
    asScala.agentPath

  @Nonnull
  lazy val directory: JExpression =
    JExpression(asScala.directoryExpr)

  @Nonnull
  def pattern: Optional[Pattern] =
    asScala.pattern.map(_.pattern).toJava

  @Nonnull
  def withRevision(revision: Optional[ItemRevision]) =
    copy(asScala.withRevision(revision.toScala))

object JFileWatch extends JJsonable.Companion[JFileWatch]:
  type AsScala = FileWatch

  @Nonnull
  def checked(
    @Nonnull id: OrderWatchPath,
    @Nonnull workflowPath: WorkflowPath,
    @Nonnull agentPath: AgentPath,
    @Nonnull directory: JExpression,
    @Nonnull pattern: Optional[String],
    @Nonnull orderIdExpression: Optional[String],
    @Nonnull delay: java.time.Duration)
  : VEither[Problem, JFileWatch] =
    (for
      pattern <- pattern.toScala.traverse(SimplePattern.checked(_))
      orderIdExpression <- orderIdExpression.toScala.traverse(parseExpression(_))
    yield
      JFileWatch(FileWatch(
        id, workflowPath, agentPath,
        directory.asScala, pattern,
        orderIdExpression,
        delay.toFiniteDuration))
      ).toVavr

  @Nonnull
  override def fromJson(jsonString: String): VEither[Problem, JFileWatch] =
    super.fromJson(jsonString)

  protected def jsonEncoder = FileWatch.jsonCodec
  protected def jsonDecoder = FileWatch.jsonCodec
