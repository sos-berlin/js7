package js7.proxy.javaapi.data.order

import io.vavr.control.{Either => VEither}
import java.util.Optional
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.agent.AgentId
import js7.data.order.{Order, OrderId}
import js7.data.value.Value
import js7.proxy.javaapi.data.common.VavrConverters._
import js7.proxy.javaapi.data.common.{JJsonable, JavaWrapper}
import js7.proxy.javaapi.data.workflow.JWorkflowId
import js7.proxy.javaapi.data.workflow.position.JWorkflowPosition
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.reflect.ClassTag

@javaApi
final case class JOrder(asScala: Order[Order.State])
extends JJsonable[JOrder]
{
  import JOrder._

  protected type AsScala = Order[Order.State]

  protected def companion = JOrder

  def id: OrderId =
    asScala.id

  def workflowPosition: JWorkflowPosition =
    JWorkflowPosition(asScala.workflowPosition)

  def workflowId: JWorkflowId =
    JWorkflowId(asScala.workflowId)

  def arguments: java.util.Map[String, Value] =
    asScala.arguments.asJava

  def parent: Optional[OrderId] =
    asScala.parent.toJava

  def attached: VEither[Problem, AgentId] =
    asScala.attached.toVavr

  def checkedState[S <: State](s: StateType[S]): VEither[Problem, S] =
    asScala.checkedState(ClassTag(s.scalaClass))
      .flatMap((o: Order[Order.State]) =>
        o.state match {
          case forked: Order.Forked => Right(Forked(forked).asInstanceOf[S])
          case Order.Finished => Right(Finished.asInstanceOf[S])
          case Order.Removed => Right(Removed.asInstanceOf[S])
          case o => Left(Problem(s"Scala Order.${o.getClass.simpleScalaName} is not available for Java"))
        })
      .toVavr
}

@javaApi
object JOrder extends JJsonable.Companion[JOrder]
{
  override def fromJson(jsonString: String): VEither[Problem, JOrder] =
    super.fromJson(jsonString)

  protected def jsonEncoder = Order.jsonEncoder
  protected def jsonDecoder = Order.jsonDecoder

  val forked = new StateType(classOf[Forked], classOf[Order.Forked])
  val finished = new StateType(Finished.getClass, Order.Finished.getClass)
  val removed = new StateType(Removed.getClass, Order.Removed.getClass)

  sealed trait State extends JavaWrapper

  sealed class StateType[S <: State](clas: Class[S], private[JOrder] val scalaClass: Class[_ <: Order.State])

  final case class Forked(asScala: Order.Forked) extends State {
    protected type AsScala = Order.Forked

    def childOrderIds: java.util.List[OrderId] =
      asScala.children.map(_.orderId).asJava
  }

  case object Finished extends State {
    protected type AsScala = Order.Finished
    val asScala = Order.Finished
  }

  case object Removed extends State {
    protected type AsScala = Order.Removed
    val asScala = Order.Removed
  }
}
