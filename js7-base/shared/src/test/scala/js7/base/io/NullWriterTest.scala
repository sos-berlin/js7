package js7.base.io

import js7.base.test.OurTestSuite

final class NullWriterTest extends OurTestSuite:

  "test" in:
    val w = NullWriter()
    w.write("test")
    w.flush()
    w.close()
