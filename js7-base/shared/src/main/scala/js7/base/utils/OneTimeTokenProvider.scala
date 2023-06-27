package js7.base.utils

import cats.effect.Resource
import monix.eval.Task

final class OneTimeTokenProvider private {

  @volatile private var current: Option[OneTimeToken] = None

  val resource: Resource[Task, OneTimeToken] =
    Resource.make(
      acquire = Task {
        val token = OneTimeToken.random()
        current = Some(token)
        token
      })(
      release = _ => Task {
        current = None
      })

  def confirms(token: OneTimeToken): Boolean =
    current.contains(token)
}

object OneTimeTokenProvider {
  /** Unsafe because it has a state. */
  def unsafe(): OneTimeTokenProvider =
    new OneTimeTokenProvider
}
