package js7.common.system

import java.time.ZoneId
import js7.base.Js7Version
import js7.base.system.ServerOperatingSystem.operatingSystem
import js7.base.time.Timestamp
import js7.common.system.JavaInformations.javaInformation
import js7.data.platform.PlatformInfo

object PlatformInfos:

  def currentPlatformInfo(): PlatformInfo = PlatformInfo(
    Timestamp.now,
    ZoneId.systemDefault.getId,
    Js7Version,
    hostname = operatingSystem.hostname,
    operatingSystem.distributionNameAndVersionOption,
    cpuModel = operatingSystem.cpuModel,
    javaInformation())
