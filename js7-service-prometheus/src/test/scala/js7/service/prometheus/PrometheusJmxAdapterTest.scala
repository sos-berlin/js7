package js7.service.prometheus

import js7.base.data.ByteArray
import js7.base.test.OurTestSuite

final class PrometheusJmxAdapterTest extends OurTestSuite:

  "Prometheus" in:
    val adapter = new PrometheusJmxAdapter
    val string = adapter.metrics[ByteArray]().utf8String
    Logger[PrometheusJmxAdapterTest].info(string)
    assert(string.startsWith(PrometheusJmxAdapter.Headline + "# HELP"))
