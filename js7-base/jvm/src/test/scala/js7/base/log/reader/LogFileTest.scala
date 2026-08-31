package js7.base.log.reader

import js7.base.data.ByteArray
import js7.base.test.OurTestSuite

final class LogFileTest extends OurTestSuite:

  "isHeaderLine" in :
    assert(LogFile.isHeaderLine(ByteArray("2026-02-24 08:05:55.244272+02:00 Begin bla\n")))
    assert(!LogFile.isHeaderLine(ByteArray("2026-02-24 08:05:55.244272+02:00 info bla\n")))
