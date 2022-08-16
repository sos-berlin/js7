package js7.data.platform

import js7.base.circeutils.CirceUtils.*
import js7.base.test.Test
import js7.base.time.Timestamp
import js7.base.version.Version
import js7.data.system.JavaInformation
import js7.tester.CirceJsonTester.testJson

final class PlatformInfoTest extends Test {

  "PlatformInfo" in {
    testJson(
      PlatformInfo(
        Timestamp("2022-07-08T12:00:00Z"),
        timezone = "Europe/Berlin",
        Version("2.4.0-TEST"),
        hostname = "HOST",
        operatingSystemDistribution = Some("DISTRIBUTION"),
        cpuModel = Some("CPU"),
        JavaInformation(
          version = "x.y.z",
          availableProcessors = 8,
          JavaInformation.Memory(maximum = 3, total = 2, free = 1),
          systemProperties = Map("test" -> "TEST"))),

    json"""{
      "timestamp": 1657281600000,
      "timezone": "Europe/Berlin",
      "js7Version": "2.4.0-TEST",
      "hostname": "HOST",
      "operatingSystemDistribution": "DISTRIBUTION",
      "cpuModel": "CPU",
      "java": {
        "version": "x.y.z",
        "availableProcessors": 8,
        "memory": {
          "maximum": 3,
          "total": 2,
          "free": 1
        },
        "systemProperties": {
          "test": "TEST"
        }
      }
    }""")
  }
}
