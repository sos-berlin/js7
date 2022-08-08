package js7.data_for_java.order

import io.vavr.control.Either as VEither
import java.time.Instant
import java.util.Optional
import javax.annotation.Nonnull
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.time.JavaTimestamp.specific.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.agent.AgentPath
import js7.data.order.{Order, OrderId}
import js7.data.subagent.SubagentId
import js7.data.value.Value
import js7.data_for_java.common.{JJsonable, JavaWrapper}
import js7.data_for_java.vavr.VavrConverters.*
import js7.data_for_java.workflow.JWorkflowId
import js7.data_for_java.workflow.position.JWorkflowPosition
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.reflect.ClassTag

@javaApi
final case class JOrder(asScala: Order[Order.State])
extends JJsonable[JOrder]
{
  import JOrder.*

  type AsScala = Order[Order.State]

  protected def companion = JOrder

  @Nonnull
  def id: OrderId =
    asScala.id

  @Nonnull
  def workflowPosition: JWorkflowPosition =
    JWorkflowPosition(asScala.workflowPosition)

  @Nonnull
  def workflowId: JWorkflowId =
    JWorkflowId(asScala.workflowId)

  @Nonnull
  def arguments: java.util.Map[String, Value] =
    asScala.arguments.asJava

  @Nonnull
  def scheduledFor: Optional[Instant] =
    asScala.scheduledFor.map(_.toInstant).toJava

  @Nonnull
  def parent: Optional[OrderId] =
    asScala.parent.toJava

  @Nonnull
  def attached: VEither[Problem, AgentPath] =
    asScala.attached.toVavr

  @Nonnull
  def checkedState[S <: State](@Nonnull s: StateType[S]): VEither[Problem, S] =
    asScala.checkedState(ClassTag(s.scalaClass))
      .flatMap((o: Order[Order.State]) =>
        o.state match {
          case forked: Order.Forked => Right(Forked(forked).asInstanceOf[S])
          case o: Order.Processing => Right(Processing(o).asInstanceOf[S])
          case Order.Finished => Right(Finished.asInstanceOf[S])
          case Order.Deleted => Right(Deleted.asInstanceOf[S])
          case o => Left(Problem(s"Scala Order.${o.getClass.simpleScalaName} is not available for Java"))
        })
      .toVavr
}

@javaApi
object JOrder extends JJsonable.Companion[JOrder]
{
  @Nonnull
  override def fromJson(@Nonnull jsonString: String): VEither[Problem, JOrder] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Order.jsonEncoder
  protected def jsonDecoder = Order.jsonDecoder

  val forked = new StateType(classOf[Forked], classOf[Order.Forked])
  val finished = new StateType(Finished.getClass, Order.Finished.getClass)
  val deleted = new StateType(Deleted.getClass, Order.Deleted.getClass)

  sealed trait State extends JavaWrapper

  sealed class StateType[S <: State](
    clas: Class[S],
    private[JOrder] val scalaClass: Class[? <: Order.State])

  final case class Forked(asScala: Order.Forked) extends State {
    type AsScala = Order.Forked

    @Nonnull
    def childOrderIds: java.util.List[OrderId] =
      asScala.children.map(_.orderId).asJava
  }

  final case class Processing(asScala: Order.Processing) extends State {
    type AsScala = Order.Processing

    /** @return empty iff written by v2.2 */
    def maybeSubagentId: Optional[SubagentId] =
      asScala.subagentId.toJava
  }

  case object Finished extends State {
    type AsScala = Order.Finished
    val asScala = Order.Finished
  }

  case object Deleted extends State {
    type AsScala = Order.Deleted
    val asScala = Order.Deleted
  }
}
