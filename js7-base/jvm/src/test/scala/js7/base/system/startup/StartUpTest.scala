package js7.base.system.startup

import js7.base.log.Logger
import js7.base.test.OurTestSuite

final class StartUpTest extends OurTestSuite:
  "Just log" in:
    Logger[this.type].info(StartUp.startUpLine("StartUpTest"))
    StartUp.logJavaSettings()
