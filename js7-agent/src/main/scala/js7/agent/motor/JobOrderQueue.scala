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
      synchronized:
        orders.foreach: order =>
          val metering = entryMeter.startMetering()
          if order.forceJobAdmission then
            forceAdmissionQueue.enqueue(order, metering)
          queue.enqueue(order, metering).foreach: removedEntry =>
            entryMeter.stopMetering(removedEntry.metering)

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

      entryMeter.stopMetering(entry.metering)
      //JobMotorsMXBean.Bean.orderQueueLength -= 1
      //JobMotorsMXBean.Bean.orderQueueNanos += entry.since.elapsed.toNanos
      entry.order

  def remove(orderId: OrderId)(using LockedForRemoval): IO[Boolean] =
    IO:
      synchronized:
        queue.remove(orderId).fold(false): entry =>
          forceAdmissionQueue.remove(orderId)
          entryMeter.stopMetering(entry.metering)
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

    /** @return removed Entry. */
    def enqueue(order: Order[IsFreshOrReady], metering: CallMeter.Metering): Option[Entry] =
      synchronized:
        val result = remove(order.id)
        queue += Entry(order, metering)
        isQueued += order.id
        result

    def dequeueNext(): Entry =
      synchronized:
        val entry = queue.remove(0)
        isQueued -= entry.order.id
        entry

    // Slow !!!
    def remove(orderId: OrderId): Option[Entry] =
      synchronized:
        if isQueued.remove(orderId) then
          meterRemove:
            queue.indexWhere(_.order.id == orderId) match
              case -1 => None
              case i => Some(queue.remove(i))
        else
          None

    def isEmpty: Boolean =
      synchronized:
        queue.isEmpty

    inline def nonEmpty: Boolean =
      !isEmpty

    override def toString =
      synchronized:
        s"MutableOrderQueue(${queue.size} orders)" //, ${inProcess.size} in process)"


  final case class Entry(order: Order[IsFreshOrReady], metering: CallMeter.Metering)
