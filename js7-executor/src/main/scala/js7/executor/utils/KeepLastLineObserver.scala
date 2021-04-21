package js7.executor.utils

import monix.reactive.Observer

private[executor] final class KeepLastLineObserver(observer: Observer[String]) extends Observer[String]
{
  private val lastLineKeeper = new LastLineKeeper

  def lastLine = lastLineKeeper.lastErrLine

  def onNext(chunk: String) = {
    lastLineKeeper.put(chunk)
    observer.onNext(chunk)
  }

  def onError(t: Throwable) = observer.onError(t)

  def onComplete() = observer.onComplete()
}
