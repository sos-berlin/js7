package js7.base.time

import js7.base.test.OurTestSuite
import js7.base.time.TimestampForTests.ts

final class TimestampForTestsTest extends OurTestSuite:

  "ts" in:
    assert(ts"2024-12-15T12:34:56.789Z" == Timestamp("2024-12-15T12:34:56.789Z"))
    assert(ts"2024-12-15t12:34:56.789z" == Timestamp("2024-12-15T12:34:56.789Z"))
    assert(ts"2024-12-15t12:34:56z" == Timestamp("2024-12-15T12:34:56Z"))
    // Sometimes, Scalac 3.5.2 crashes with: Failed to find name hashes for js7.base.time.TimestampForTestsTest
    //assertDoesNotCompile(""" ts"2024-12-15T12:34:56.789" """)
