package js7.launcher.utils

import monix.reactive.Observer

private[launcher] final class KeepLastLineObserver(observer: Observer[String]) extends Observer[String]:
  private val lastLineKeeper = new LastLineKeeper

  def lastLine = lastLineKeeper.lastErrLine

  def onNext(chunk: String) =
    lastLineKeeper.put(chunk)
    observer.onNext(chunk)

  def onError(t: Throwable) = observer.onError(t)

  def onComplete() = observer.onComplete()
