package js7.base.config

import cats.effect.{IO, Resource, ResourceIO, Sync}
import com.typesafe.config.Config
import js7.base.catsutils.CatsEffectExtensions.unsafeRuntime
import js7.base.catsutils.{Environment, OurIORuntimeRegister}
import js7.base.fs2utils.ByteChunksLineSplitter.MinimumLength

final case class Js7Conf(
  logFileIndexLineLength: Int,
  config: Config)


object Js7Conf:

  def fromConfig(config: Config): Js7Conf =
    val logFileIndexLineLength = config.getBytes("js7.log.index.max-bytes-per-line")
    if logFileIndexLineLength <= MinimumLength || logFileIndexLineLength > Int.MaxValue then
      throw new IllegalArgumentException(
        s"js7.log.index.max-bytes-per-line must be > ${MinimumLength} and <= ${Int.MaxValue}")
    Js7Conf(
      logFileIndexLineLength = logFileIndexLineLength.toInt,
      config)

  def registerInEnvironment(config: Config): ResourceIO[Unit] =
    Resource.suspend:
      IO.unsafeRuntime.map: ioRuntime =>
        val env = OurIORuntimeRegister.toEnvironment(ioRuntime)
        registerInEnvironment[IO](env, config)

  def registerInEnvironment[F[_]: Sync](env: Environment, config: Config): Resource[F, Unit] =
    for
      _ <- env.registerPure[F, Config](config, ignoreDuplicate = true)
      _ <- env.registerPure[F, Js7Conf](Js7Conf.fromConfig(config), ignoreDuplicate = true)
    yield ()
