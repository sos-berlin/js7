package js7.service.prometheus

import js7.base.data.ByteArray
import js7.base.test.OurTestSuite
import js7.base.time.ScalaTime.RichDeadline
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
    assert(string.startsWith(PrometheusJmxAdapter.Headline + "# HELP"))

  "Speed" in:
    if isIntelliJIdea then
      val adapter = new PrometheusJmxAdapter
      var dummy: Any = null
      val n = 1000
      val t = Deadline.now
      (1 to n).foreach: _ =>
        dummy = adapter.metrics[ByteString]()
      logger.info(itemsPerSecondString(t.elapsed, n, "calls"))
