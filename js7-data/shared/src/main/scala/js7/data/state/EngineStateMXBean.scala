package js7.data.state

import cats.effect.ResourceIO
import java.util.Map as JMap
import js7.base.system.MBeanUtils.registerStaticMBean
import js7.base.utils.MoreJavaConverters.*
import js7.base.utils.ScalaUtils.syntax.RichJavaClass

trait EngineStateMXBean:
  this: EngineStateMXBean.Bean =>

  def getClusterStateCode: Int =
    stateView.clusterState.asCode.index

  def getOrderCount: Int =
    stateView.idToOrder.size

  def getOrderStatusToCount: JMap[String, java.lang.Integer] =
    stateView.idToOrder
      .groupMapReduce(_._2.state.getClass.cachedSimpleScalaName)(_ => 1)(_ + _)
      .view.mapValues(Int.box).asJava

  def getStatisticsEventToTotal: JMap[String, java.lang.Long] =
    stateView.statistics.eventCounter.eventToCount
      .view.mapValues(Long.box).asJava

  def getStatisticsEventTotal: Long =
    stateView.statistics.eventCounter.totalEventCount


object EngineStateMXBean:
  private val bean = new Bean

  export bean.setEngineState

  // "inline" hides method from JMX
  inline def register: ResourceIO[EngineStateMXBean] =
    registerStaticMBean("EngineState", bean)

  private final class Bean private[EngineStateMXBean] extends EngineStateMXBean:
    protected var stateView: StateView = StateView.empty

    // "inline" hides method from JMX
    inline def setEngineState(o: StateView): Unit =
      stateView = o
