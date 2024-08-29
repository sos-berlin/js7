package js7.base.system

import js7.base.test.OurTestSuite
import js7.base.utils.ByteUnits.toKiBGiB

final class MXBeanMapViewTest extends OurTestSuite:

  "supportedMXBeans" in:
    for (name, bean) <- MXBeanMapView.mxBeanMapView do
      for (k, v) <- bean do Logger.info(s"$name.$k=$v")

    val totalMemorySize = MXBeanMapView
      .mxBeanMapView("com.sun.management.OperatingSystemMXBean")("totalMemorySize")
      .asInstanceOf[Long]
    Logger.info(s"totalMemorySize=${toKiBGiB(totalMemorySize)}")
    assert(totalMemorySize >= 10_000_000)
