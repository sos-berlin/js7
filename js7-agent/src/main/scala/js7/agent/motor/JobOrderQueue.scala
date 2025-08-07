package js7.agent.motor

import cats.effect.IO
import js7.agent.motor.JobOrderQueue.*
import js7.base.metering.CallMeter
import js7.base.monixutils.SimpleLock
import js7.data.order.Order.IsFreshOrReady
import js7.data.order.{Order, OrderId}
import scala.annotation.implicitNotFound
import scala.collection.mutable

private final class JobOrderQueue:
  private val queue = new MutableOrderQueue
  private val forceAdmissionQueue = new MutableOrderQueue
  private val removalLock = SimpleLock[IO]

  def enqueue(orders: Seq[Order[IsFreshOrReady]]): IO[Unit] =
    IO:
      //JobMotorsMXBean.Bean.orderQueueLength += orders.length
      val metering = entryMeter.startMetering(orders.length)
      synchronized:
        orders.foreach: order =>
          if order.forceJobAdmission then
            forceAdmissionQueue.enqueue(order, metering)
          queue.enqueue(order, metering)

  /** Guard a sequence of isEmpty, dequeueNextOrder and isEmpty for atomar behaviour. */
  def lockForRemoval[A](body: LockedForRemoval ?=> IO[A]): IO[A] =
    removalLock.surround:
      body(using LockedForRemoval)

  def dequeueNextOrder(onlyForcedAdmission: Boolean)(using LockedForRemoval)
  : Order[IsFreshOrReady] =
    synchronized:
      val entry =
        if onlyForcedAdmission then
          val entry = forceAdmissionQueue.dequeueNext()
          queue.remove(entry.order.id)
          entry
        else
          val entry = queue.dequeueNext()
          forceAdmissionQueue.remove(entry.order.id)
          entry

      entry.metering.close()
      //JobMotorsMXBean.Bean.orderQueueLength -= 1
      //JobMotorsMXBean.Bean.orderQueueNanos += entry.since.elapsed.toNanos
      entry.order

  def remove(orderId: OrderId)(using LockedForRemoval): IO[Boolean] =
    IO:
      synchronized:
        queue.remove(orderId) && locally:
          forceAdmissionQueue.remove(orderId)
          //JobMotorsMXBean.Bean.orderQueueLength -= 1
          true

  def isEmpty(onlyForcedAdmission: Boolean)(using LockedForRemoval): Boolean =
    if onlyForcedAdmission then
      forceAdmissionQueue.isEmpty
    else
      queue.isEmpty

  def isEmptyUnsafe: Boolean =
    queue.isEmpty


private object JobOrderQueue:

  private val meterRemove = CallMeter("JobOrderQueue.MutableOrderQueue.remove")
  private val entryMeter = CallMeter("JobOrderQueue.Entry")

  @implicitNotFound("Code must be wrapped in lockForRemoval")
  sealed trait LockedForRemoval
  private object LockedForRemoval extends LockedForRemoval


  private final class MutableOrderQueue:
    private val queue = mutable.ListBuffer.empty[Entry]
    // isQueued is for optimisation
    private val isQueued = mutable.Set.empty[OrderId]

    def enqueue(order: Order[IsFreshOrReady], metering: CallMeter.Metering): Unit =
      synchronized:
        remove(order.id)
        queue += Entry(order, metering)
        isQueued += order.id

    def dequeueNext(): Entry =
      synchronized:
        val entry = queue.remove(0)
        isQueued -= entry.order.id
        entry

    // Slow !!!
    def remove(orderId: OrderId): Boolean =
      synchronized:
        isQueued.remove(orderId) && locally:
          meterRemove:
            queue.indexWhere(_.order.id == orderId) match
              case -1 => false
              case i => queue.remove(i); true

    def isEmpty: Boolean =
      synchronized:
        queue.isEmpty

    def nonEmpty: Boolean =
      !isEmpty

    override def toString =
      synchronized:
        s"MutableOrderQueue(${queue.size} orders)" //, ${inProcess.size} in process)"


  final case class Entry(order: Order[IsFreshOrReady], metering: CallMeter.Metering)
