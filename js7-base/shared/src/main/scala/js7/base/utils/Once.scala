package js7.base.utils

final class Once:
  private var once = false

  def apply(body: => Unit): Unit =
    if !once then
      synchronized:
        if !once then
          body
          once = true
