package js7.base.metering

import cats.effect.{Resource, Sync}
import java.lang.management.MemoryNotificationInfo.{MEMORY_COLLECTION_THRESHOLD_EXCEEDED, MEMORY_THRESHOLD_EXCEEDED}
import java.lang.management.{ManagementFactory, MemoryType, MemoryUsage}
import javax.management.{Notification, NotificationEmitter, NotificationListener}
import js7.base.log.Logger
import js7.base.utils.ByteUnits.toKiBGiB
import js7.base.utils.CatsUtils.syntax.RichResource
import js7.base.utils.ScalaUtils.syntax.*
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

// The name is as long as HeapMBean for readable logging layout.
object HeapWatch:
  private val logger = Logger[this.type]
  private val memoryBean = ManagementFactory.getMemoryMXBean
  private val MemoryNotificationThreshold = 0.9
  private val MinimumMemory = 10 * 1024 * 1024

  private val listener = new NotificationListener:
    def handleNotification(not: Notification, handback: AnyRef): Unit =
      logger.whenDebugEnabled:
        //val poolName = Try(MemoryNotificationInfo.from(not.getUserData.asInstanceOf))
        //  .toOption.fold("(unknown pool)")(_.getPoolName)
        val usage = memoryBean.getHeapMemoryUsage
        val used = usage.getUsed
        val max = usage.getMax
        val committed = usage.getCommitted
        // Same logging layout as in HeapMXBean
        not.getType match
          case MEMORY_COLLECTION_THRESHOLD_EXCEEDED =>
            logger.debug(s"♻️  ${memoryUsageToString(usage)
            } · Heap is nearly exhausted after garbage collection")
          case MEMORY_THRESHOLD_EXCEEDED | _ =>
            logger.debug(s"♻️  ${memoryUsageToString(usage)} · ${not.getType}")

  def resourceForMainApp[F[_]: Sync as F]: Resource[F, Unit] =
    Resource.make(
      acquire =
        F.delay:
          try
            val memoryPoolBeans = ManagementFactory.getMemoryPoolMXBeans.asScala.flatMap: bean =>
              ((bean.getType eq MemoryType.HEAP)
                && (bean.isCollectionUsageThresholdSupported || bean.isUsageThresholdSupported)
              ).thenMaybe:
                val max = bean.getUsage.getMax
                val threshold = (max * MemoryNotificationThreshold).toLong.min(max - MinimumMemory)
                threshold > 0 thenSome:
                  val remainder = s"(threshold=${
                    (threshold * 100).toInt
                  }%=${
                    toKiBGiB((MemoryNotificationThreshold * max).toLong)
                  } KiB) max=${toKiBGiB(max)}"

                  if bean.isUsageThresholdSupported then
                    logger.debug:
                      s"MemoryPool(${bean.getName}).setUsageThreshold(threshold=$remainder"
                    bean.setUsageThreshold(threshold)

                  if bean.isCollectionUsageThresholdSupported then
                    logger.debug:
                      s"MemoryPool(${bean.getName}).setCollectionUsageThreshold(threshold=$remainder"
                    bean.setCollectionUsageThreshold(threshold)

                  bean
            memoryBean.asInstanceOf[NotificationEmitter]
              .addNotificationListener(listener, null, null)
            memoryPoolBeans
          catch case NonFatal(t) =>
            logger.warn(t.toStringWithCauses, t)
            Nil)(
      release = memoryPoolBeans =>
        F.delay:
          try
            memoryBean.asInstanceOf[NotificationEmitter]
              .removeNotificationListener(listener, null, null)
            memoryPoolBeans.foreach: bean =>
              if bean.isUsageThresholdSupported then
                bean.setUsageThreshold(0)
              if bean.isCollectionUsageThresholdSupported then
                bean.setCollectionUsageThreshold(0)
          catch case NonFatal(t) =>
            logger.debug(t.toStringWithCauses, t)
    ).void

  private[metering] def memoryUsageToString(usage: MemoryUsage): String =
    memoryUsageToString(usage, usage.getUsed)

  private[metering] def memoryUsageToString(usage: MemoryUsage, used: Long): String =
    val usage = memoryBean.getHeapMemoryUsage
    val max = usage.getMax
    val committed = usage.getCommitted
    s"${
      toKiBGiB(used)
    } is used, ${
      (used.toDouble / committed * 100).toLong
    }% of ${toKiBGiB(committed)} ${
      if committed == max then
        "commited and maximum"
      else
        s"committed, ${
          (used.toDouble / max * 100).toLong
        }% of ${toKiBGiB(max)} maximum"
    }"
