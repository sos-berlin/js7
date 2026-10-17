package js7.base.metering

import cats.effect.kernel.{Resource, Sync}
import cats.effect.{IO, ResourceIO}
import com.sun.management.GarbageCollectionNotificationInfo
import com.sun.management.GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION
import java.lang.management.{ManagementFactory, MemoryType}
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}
import js7.base.bean.MBeanUtils
import js7.base.bean.MBeanUtils.registerMBean
import js7.base.log.Logger
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*

trait HeapMXBean:
  /** The total heap usage after each last garbage collection. */
  def getUsedAfterGC: java.lang.Long


final class HeapMBean extends HeapMXBean:
  var getUsedAfterGC = null.asInstanceOf


object HeapMBean:
  private val logger = Logger[this.type]

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
            val listener = newNotificationListener: (used, isMajor) =>
              val fraction = used / sys.runtime.maxMemory.toDouble
              bean.getUsedAfterGC = used
              if isMajor then
                logger.info(s"♻️  ${if isMajor then "Major" else "Minor"
                  } garbage collection: ${toKiBGiB(used)} or ${fraction * 100}% is used")
            emitter.addNotificationListener(
              listener, _.getType == GARBAGE_COLLECTION_NOTIFICATION, null)
            Some(emitter -> listener)
          case _ =>
            None)(
        release =
          _.foldMapMI: (emitter, listener) =>
            F.delay:
              emitter.removeNotificationListener(listener)
      ).void

  private def newNotificationListener(onGc: (usage: Long, isMajor: Boolean) => Unit)
  : NotificationListener =
    val memoryMXBean = ManagementFactory.getMemoryMXBean
    val isHeapPoolName = ManagementFactory.getMemoryPoolMXBeans.asScala
      .filter(_.getType == MemoryType.HEAP)
      .map(_.getName)
      .toSet
    (notification: Notification, handback: Any) =>
      val info = GarbageCollectionNotificationInfo.from:
        notification.getUserData.asInstanceOf[CompositeData]
      val gcName = info.getGcName
      val gcAction = info.getGcAction // Ex: "end of major GC"
      // Expected values for gcName:
      //   "G1 Old Generation", "G1 Young Generation"
      //   We use G1, or SerialGC when running on a single thread
      // Expected values for gcAction: "end of minor GC", "end of major GC", "end of full GC"
      var used = 0L
      info.getGcInfo.getMemoryUsageAfterGc.forEach: (poolName, usage) =>
        if isHeapPoolName.contains(poolName) then
          used += usage.getUsed
          //if poolName.contains("Old") || poolName.contains("Tenured") then
          //  println(f"  $poolName%s efter städning: ${toKiBGib(usage.getUsed)}%.2f MB")
      logger.trace(s"handleNotification: used=${toKiBGiB(used)} · $gcName: $gcAction · cause: ${
        info.getGcCause}")
      onGc(used,
        isMajor = gcAction.contains("major") || gcName.contains("ZGC")
          || gcName.contains("MarkSweep"))
