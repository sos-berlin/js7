package js7.launcher

import js7.base.test.OurAsyncTestSuite
import js7.base.utils.ScalaUtils.syntax.*
import js7.launcher.StdObserversForTest.testSink

final class StdObserversTest extends OurAsyncTestSuite:

  "errorLine useErrorLineLengthMax=false" in:
    runExample(keepLastErrLine = false, expectErrLine = None)

  "errorLine useErrorLineLengthMax=true" in:
    runExample(keepLastErrLine = true, expectErrLine = Some("LAST"))

  private def runExample(keepLastErrLine: Boolean, expectErrLine: Option[String]) =
    StdObservers
      .testSink(useErrorLineLengthMax = keepLastErrLine ?  1024, name = "StdObserversTest")
      .use: testSink =>
        for
          _ <- testSink.stdObservers.err.write("LAST\n")
        yield
          // Return result delayed in an IO
          testSink.err.map(_ -> testSink.stdObservers.errorLine)
      .flatten
      .map: (err, errorLine) =>
        assert(err == "LAST\n" && errorLine == expectErrLine)
