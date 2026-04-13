package js7.base.log.reader

import cats.effect.SyncIO
import cats.effect.kernel.{Resource, Sync}
import cats.syntax.functor.*
import java.io.IOException
import java.nio.file.{Files, Path}
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.io.file.FileUtils.syntax.RichPath
import js7.base.log.reader.LogDirectoryMXBean.*
import js7.base.metering.CallMeter
import js7.base.system.MBeanUtils.registerMBean

sealed trait LogDirectoryMXBean:
  this: Bean =>

  def getSize: java.lang.Long =
    // SLOW
    meterLogDirectorySize:
      try
        logDirectory.directoryStream[SyncIO].map: path =>
          try Files.size(path)
          catch case _: IOException => 0
        .compile.fold(0L)(_ + _)
        .run()
      catch case _: IOException => null.asInstanceOf[java.lang.Long]


object LogDirectoryMXBean:
  private val meterLogDirectorySize = CallMeter("LogDirectoryMXBean.logDirectorySize")

  // "inline" hides method from JMX
  inline def register[F[_]: Sync as F](logDirectory: Path): Resource[F, Unit] =
    registerMBean[F, LogDirectoryMXBean]("LogDirectory"):
      F.pure(Bean(logDirectory))
    .void

  final class Bean(protected val logDirectory: Path) extends LogDirectoryMXBean

