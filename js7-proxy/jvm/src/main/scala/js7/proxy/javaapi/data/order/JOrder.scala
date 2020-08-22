package js7.proxy.javaapi.data.order

import io.vavr.control.{Either => VEither}
import java.util.Optional
import js7.base.annotation.javaApi
import js7.base.problem.Problem
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import js7.data.agent.AgentRefPath
import js7.data.order.{Order, OrderId}
import js7.proxy.javaapi.data.common.VavrConverters._
import js7.proxy.javaapi.data.common.{JJsonable, JavaWrapper}
import js7.proxy.javaapi.data.order.JOrder.{Forked, State, StateType}
import js7.proxy.javaapi.data.workflow.JWorkflowId
import js7.proxy.javaapi.data.workflow.position.{JPosition, JWorkflowPosition}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.reflect.ClassTag

@javaApi
final case class JOrder(underlying: Order[Order.State])
extends JJsonable[JOrder]
{
  def workflowPosition: JWorkflowPosition =
    JWorkflowPosition(underlying.workflowPosition)

  protected type Underlying = Order[Order.State]

  protected def companion = JOrder

  def id: OrderId =
    underlying.id

  def workflowId: JWorkflowId =
    JWorkflowId(underlying.workflowId)

  def position: JPosition =
    JPosition(underlying.position)

  def arguments: java.util.Map[String, String] =
    underlying.arguments.asJava

  def parent: Optional[OrderId] =
    underlying.parent.toJava

  def attached: VEither[Problem, AgentRefPath] =
    underlying.attached.toVavr

  def checkedState[S <: State](s: StateType[S]): VEither[Problem, S] =
    underlying.checkedState(ClassTag(s.underlyingClass))
      .flatMap((o: Order[Order.State]) =>
        o.state match {
          case forked: Order.Forked => Right(Forked(forked).asInstanceOf[S])
          case o => Left(Problem(s"Scala Order.${o.getClass.simpleScalaName} is not available for Java"))
        })
      .toVavr
}

@javaApi
object JOrder extends JJsonable.Companion[JOrder]
{
  override def fromJson(jsonString: String): VEither[Problem, JOrder] =
    super.fromJson(jsonString)

  val jsonEncoder = Order.jsonEncoder
  val jsonDecoder = Order.jsonDecoder

  sealed trait State extends JavaWrapper

  sealed class StateType[S <: State](clas: Class[S], private[JOrder] val underlyingClass: Class[_ <: Order.State])

  final case class Forked(underlying: Order.Forked) extends State {
    protected type Underlying = Order.Forked

    def childOrderIds: java.util.List[OrderId] =
      underlying.children.map(_.orderId).asJava
  }
  val forked = new StateType(classOf[Forked], classOf[Order.Forked])
}
