package js7.base.utils

import cats.effect.{IO, Resource}

final class OneTimeTokenProvider private():

  @volatile private var current: Option[OneTimeToken] = None

  val resource: Resource[IO, OneTimeToken] =
    Resource.make(
      acquire = IO {
        val token = OneTimeToken.random()
        current = Some(token)
        token
      })(
      release = _ => IO {
        current = None
      })

  def confirms(token: OneTimeToken): Boolean =
    current.contains(token)


object OneTimeTokenProvider:
  /** Unsafe because it has a state. */
  def unsafe(): OneTimeTokenProvider =
    new OneTimeTokenProvider
