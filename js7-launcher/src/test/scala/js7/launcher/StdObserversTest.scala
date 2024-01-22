package js7.launcher

import js7.base.catsutils.CatsEffectExtensions.joinStd
import js7.base.test.OurAsyncTestSuite

final class StdObserversTest extends OurAsyncTestSuite:

  "errorLine keepLastErrLine=false" in:
    runExample(keepLastErrLine = false, expectErrLine = None)

  "errorLine keepLastErrLine=true" in:
    runExample(keepLastErrLine = true, expectErrLine = Some("LAST"))

  private def runExample(keepLastErrLine: Boolean, expectErrLine: Option[String]) =
    StdObservers
      .resource(charBufferSize = 4096, keepLastErrLine = keepLastErrLine)
      .use: stdObservers =>
        for
          draining <- stdObservers.errStream.compile.toList.start
          _ <- stdObservers.err.write("LAST\n")
          _ <- stdObservers.errChannel.close
          errList <- draining.joinStd
        yield assert:
          errList == List("LAST\n") && stdObservers.errorLine == expectErrLine
