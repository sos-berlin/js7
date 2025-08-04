package js7.agent.motor

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import js7.base.utils.Atomic

private sealed trait JobMotorsMXBean:
  def getTotalOrderQueueLength: Int
  /** Sum of queuing durations. */
  def getTotalOrderQueueSeconds: Double


private object JobMotorsMXBean:
  object Bean extends JobMotorsMXBean:
    val orderQueueLength: AtomicInteger = Atomic(0)
    val orderQueueNanos: AtomicLong = Atomic(0L)
    def getTotalOrderQueueLength = orderQueueLength.get
    def getTotalOrderQueueSeconds = orderQueueNanos.get.toDouble
