package js7.service.prometheus

import java.lang.management.ManagementFactory
import javax.management.ObjectName
import js7.base.data.ByteArray
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.*
import js7.base.time.Stopwatch.itemsPerSecondString
import js7.base.utils.Tests.isIntelliJIdea
import js7.common.pekkoutils.ByteStrings.syntax.*
import org.apache.pekko.util.ByteString
import scala.concurrent.duration.Deadline

final class PrometheusJmxAdapterTest extends OurTestSuite:

  private val logger = Logger[this.type]

  "Prometheus" in:
    val adapter = new PrometheusJmxAdapter
    val string = adapter.metrics[ByteArray]().utf8String
    logger.info(string)
    assert(string.startsWith("# HELP"))

  "Speed" in:
    if isIntelliJIdea then
      val adapter = new PrometheusJmxAdapter
      var dummy: Any = null
      val n = 1000
      val t = Deadline.now
      (1 to n).foreach: _ =>
        dummy = adapter.metrics[ByteString]()
      logger.info(itemsPerSecondString(t.elapsed, n, "calls"))

  "Allocations" - {
    "Allocation per scrape and per MBean" in:
      if isIntelliJIdea then
        val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
        def allocated(body: => Unit): Long =
          val a = threads.getCurrentThreadAllocatedBytes
          body
          threads.getCurrentThreadAllocatedBytes - a

        val adapter = new PrometheusJmxAdapter
        val n = 50
        (1 to 20).foreach(_ => adapter.metrics[ByteArray]()) // warm up
        val total = allocated((1 to n).foreach(_ => adapter.metrics[ByteArray]())) / n
        logger.info(s"adapter.metrics: $total bytes allocated per call, result size is ${
          adapter.metrics[ByteArray]().length} bytes")

        val server = ManagementFactory.getPlatformMBeanServer
        import scala.jdk.CollectionConverters.*
        val patterns = Seq(
          "java.lang:name=G1 *,type=GarbageCollector", "java.lang:type=Memory",
          "java.lang:type=OperatingSystem", "java.lang:type=Runtime", "java.lang:type=Threading",
          "js7:*", "joc:*")
        val excluded = Map(
          "java.lang:type=Runtime" -> Set("InputArguments", "SystemProperties"),
          "java.lang:type=Threading" -> Set("AllThreadIds"),
          "java.lang:type=OperatingSystem" -> Set(
            "SystemCpuLoad", "FreePhysicalMemorySize", "TotalPhysicalMemorySize",
            "CpuLoad", "FreeMemorySize", "FreeSwapSpaceSize", "ProcessCpuLoad", "TotalMemorySize", "TotalSwapSpaceSize"))
        for pattern <- patterns; name <- server.queryNames(ObjectName(pattern), null).asScala.toSeq.sortBy(_.toString) do
          val attrs = server.getMBeanInfo(name).getAttributes.map(_.getName)
          val kept = attrs.filterNot(excluded.getOrElse(name.toString, Set.empty))
          def measure(names: Array[String]) =
            (1 to 10).foreach(_ => server.getAttributes(name, names))
            allocated((1 to n).foreach(_ => server.getAttributes(name, names))) / n
          val all = measure(attrs)
          val some = measure(kept)
          logger.info(f"$name%-60s all=$all%8d bytes  withoutExcluded=$some%8d bytes")

    "Allocation per Runtime attribute" in:
      testAllocsPerAttribute(ObjectName("java.lang:type=Runtime"))

    "Allocation per OperatingSystem attribute" in:
      testAllocsPerAttribute(ObjectName("java.lang:type=OperatingSystem"))

    def testAllocsPerAttribute(name: ObjectName): Unit =
      if isIntelliJIdea then
        val threads = ManagementFactory.getThreadMXBean.asInstanceOf[com.sun.management.ThreadMXBean]
        val server = ManagementFactory.getPlatformMBeanServer
        for attr <- server.getMBeanInfo(name).getAttributes.map(_.getName).sorted do
          val names = Array(attr)
          (1 to 10).foreach(_ => server.getAttributes(name, names))
          val a = threads.getCurrentThreadAllocatedBytes
          (1 to 50).foreach(_ => server.getAttributes(name, names))
          logger.info(f"$attr%-40s ${(threads.getCurrentThreadAllocatedBytes - a) / 50}%8d bytes")
  }
