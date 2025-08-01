package js7.data.controller

import cats.effect.ResourceIO
import js7.base.system.MBeanUtils.registerMBean
import js7.base.utils.MoreJavaConverters.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass
import scala.jdk.CollectionConverters.*

trait ControllerStateMXBean:
  this: ControllerStateMXBean.Bean =>

  def getOrderCount: Int =
    controllerState.idToOrder.size

  def getEventToTotal: java.util.Map[String, java.lang.Long] =
    controllerState.eventCounter.eventToCount.view.mapValues(Long.box).asJava

  def getEventTotal: Long =
    controllerState.eventCounter.totalEventCount

  def getOrderStatusToCount: java.util.Map[String, java.lang.Integer] =
    controllerState.idToOrder
      .groupMapReduce(_._2.state.getClass)(_ => 1)(_ + _)
      .view.map: (cls, n) =>
        cls.simpleScalaName -> Int.box(n)
      .toMap
      .asJava


object ControllerStateMXBean:
  private val bean = new Bean

  export bean.setControllerState

  // "inline" hides method from JMX
  inline def register: ResourceIO[ControllerStateMXBean] =
    registerMBean("EngineState", bean)

  private final class Bean private[ControllerStateMXBean] extends ControllerStateMXBean:
    protected var controllerState = ControllerState.empty

    // "inline" hides method from JMX
    inline def setControllerState(o: ControllerState): Unit =
      controllerState = o
