package js7.launcher

import js7.base.test.Test
import js7.base.thread.Futures.implicits.SuccessFuture
import js7.base.time.ScalaTime.*
import monix.reactive.subjects.PublishSubject

final class StdObserversTest extends Test
{
  "errorLine keepLastErrLine=false" in {
    val out = PublishSubject[String]()
    val err = PublishSubject[String]()

    err.completedL.startAndForget
    val stdObservers = new StdObservers(out, err, charBufferSize = 4096, keepLastErrLine = false)
    stdObservers.err.onNext("LAST\n").await(9.s)
    assert(stdObservers.errorLine.isEmpty)
  }

  "errorLine keepLastErrLine=true" in {
    val out = PublishSubject[String]()
    val err = PublishSubject[String]()

    err.completedL.startAndForget
    val stdObservers = new StdObservers(out, err, charBufferSize = 4096, keepLastErrLine = true)
    stdObservers.err.onNext("LAST\n").await(9.s)
    assert(stdObservers.errorLine == Some("LAST"))
  }
}
