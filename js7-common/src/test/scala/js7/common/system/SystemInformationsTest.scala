package js7.common.system

import io.circe.syntax.EncoderOps
import js7.base.system.{SystemInformation, SystemInformations}
import js7.base.test.OurTestSuite

/**
  * @author Joacim Zschimmer
  */
final class SystemInformationsTest extends OurTestSuite:

  // See also js7.base.system.SystemInformationTest

  "operatingSystem" in:
    val expectedOsKeys = Set(
      "freeSwapSpaceSize",
      "freePhysicalMemorySize",
      "freeMemorySize",
      "totalMemorySize",
      "cpuLoad",
      "committedVirtualMemorySize",
      "processCpuTime",
      "totalPhysicalMemorySize",
      "processCpuLoad",
      "totalSwapSpaceSize")
    val os = SystemInformations.systemInformation()
      .mxBeans("operatingSystem").keySet
    assert(expectedOsKeys.subsetOf(os))

  "JSON" in:
    val info = SystemInformations.systemInformation()
    Logger.info(s"systemInformation=$info")

    assert(removeNaN(info).asJson.as[SystemInformation] == Right(removeNaN(info)))


  // Double.NaN is not comparable, so we remove it
  private def removeNaN(info: SystemInformation): SystemInformation =
    info.copy(
      mxBeans = info.mxBeans
        .view.mapValues: bean =>
          bean.view.filter:
            case (_, v: Double) => !v.isNaN
            case _ => true
          .toMap
        .toMap)
