package js7.launcher.forwindows

import js7.base.utils.Atomic

/**
  * Once `releaseAfterUse` has been called and no one uses the resource, `release` ist called.
  * Thread-safe.
  */
private final class ResourceGuard[A] private(resource: A, release: A => Unit):

  private var usage = 1
  private val _releaseAfterUse = Atomic(false)

  def use[B](body: Option[A] => B): B =
    if increment() > 0 then
      try body(Some(resource))
      finally decrement()
    else
      body(None)

  def releaseAfterUse(): Unit =
    if !_releaseAfterUse.getAndSet(true) then
      decrement()

  private def increment(): Int =
    synchronized:
      if usage > 0 then  // We don't increment 0. 0 means released.
        usage += 1
      usage

  private def decrement(): Unit =
    synchronized:
      assert(usage > 0)
      usage -= 1
      usage
    match
      case 0 => release(resource)
      case _ =>

private object ResourceGuard:
  def apply[A](resource: A)(release: A => Unit) =
    new ResourceGuard[A](resource, release)
