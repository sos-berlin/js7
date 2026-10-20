package js7.base.metering

import cats.effect.kernel.{Resource, Sync}
import cats.effect.{IO, ResourceIO}
import cats.syntax.foldable.*
import com.sun.management.GarbageCollectionNotificationInfo
import com.sun.management.GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION
import java.lang.management.{ManagementFactory, MemoryType}
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}
import js7.base.bean.MBeanUtils
import js7.base.bean.MBeanUtils.registerMBean
import js7.base.log.Logger.syntax.*
import js7.base.log.{LogLevel, Logger}
import js7.base.utils.CatsUtils.syntax.RichResource
import scala.jdk.CollectionConverters.*

trait HeapMXBean:
  /** The total heap usage after last garbage collection. */
  def getUsedAfterGC: Long


final class HeapMBean extends HeapMXBean:
  var getUsedAfterGC = 0


object HeapMBean:
  private val logger = Logger[this.type]
  private val memoryBean = ManagementFactory.getMemoryMXBean
  private val meterNotificationListener = CallMeter("HeapMBean.NotificationListener")

  def register: ResourceIO[HeapMXBean] =
    for
      bean <- registerMBean("Heap")(IO(new HeapMBean))
      _ <- runGcListener[IO](bean)
    yield
      bean

  private def runGcListener[F[_]: Sync as F](bean: HeapMBean): Resource[F, Unit] =
    Resource.make(
      acquire = F.delay:
        val gcBeans = ManagementFactory.getGarbageCollectorMXBeans.asScala
        gcBeans.toVector.flatMap:
          case emitter: NotificationEmitter =>
            val listener = newNotificationListener: kind =>
              val usage = memoryBean.getHeapMemoryUsage
              val used = usage.getUsed
              bean.getUsedAfterGC = used
              if logger.isEnabled(kind.logLevel) then
                val committed = usage.getCommitted
                val max = usage.getMax
                logger.log(kind.logLevel,
                  // Same layout as in HeapWatch
                  s"♻️  ${
                    HeapWatch.memoryUsageToString(usage, used = used)
                  } · $kind garbage collection")
            emitter.addNotificationListener(
              listener, _.getType == GARBAGE_COLLECTION_NOTIFICATION, null)
            Some(emitter -> listener)
          case _ =>
            None)(
        release =
          _.foldMapM: (emitter, listener) =>
            F.delay:
              emitter.removeNotificationListener(listener)
      ).void

  private def newNotificationListener(onGc: Kind => Unit)
  : NotificationListener =
    val isHeapPoolName = ManagementFactory.getMemoryPoolMXBeans.asScala
      .filter(_.getType == MemoryType.HEAP)
      .map(_.getName)
      .toSet
    (notification: Notification, handback: Any) =>
      meterNotificationListener:
        val gcAction = notification.getUserData match
          case o: GarbageCollectionNotificationInfo => o.getGcAction
          case o: CompositeData => o.get("gcAction").toString
        // Expected values for getGcName:
        //   "G1 Old Generation", "G1 Young Generation"
        //   We use G1, or SerialGC when running on a single thread
        val kind =
          if gcAction.contains("concurrent") then
            Kind.Concurrent
          else if gcAction.contains("minor") then // "end of minor GC"
            Kind.Minor
          else if gcAction.contains("major") then // "end of major GC"
            Kind.Major
          else if gcAction.contains("full") then // "end of full GC"
            Kind.Full
          else
            null
        if kind != null then
          //We read used from MemoryMXBean: It is faster.
          //var used = 0L
          //info.getGcInfo.getMemoryUsageAfterGc.forEach: (poolName, usage) =>
          //  if isHeapPoolName.contains(poolName) then
          //    //logger.trace(s"'$poolName' efter städning: used=${toKiBGiB(usage.getUsed)}")
          //    used += usage.getUsed
          onGc(kind)

  private enum Kind(val logLevel: LogLevel):
    case Full extends Kind(LogLevel.Debug)
    case Major extends Kind(LogLevel.Debug)
    case Minor extends Kind(LogLevel.None)
    case Concurrent extends Kind(LogLevel.None)
    //case Unknown extends Kind(LogLevel.None, label: String)
