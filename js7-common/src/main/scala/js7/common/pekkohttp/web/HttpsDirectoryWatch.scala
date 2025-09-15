package js7.common.pekkohttp.web

import cats.effect.{IO, ResourceIO}
import cats.syntax.all.*
import java.nio.file.Path
import js7.base.fs2utils.StreamExtensions.*
import js7.base.io.file.watch.{DirectoryStateJvm, DirectoryWatch, DirectoryWatchSettings}
import js7.base.log.Logger
import js7.base.monixlike.MonixLikeExtensions.onErrorTap
import js7.base.service.Service
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.pekkohttp.web.HttpsDirectoryWatch.*

private final class HttpsDirectoryWatch private(
  settings: DirectoryWatchSettings,
  files: Seq[Path],
  onHttpsKeyOrCertChanged: IO[Unit])
extends Service.StoppableByRequest:

  protected def start =
    watchDirectories.start
      .flatMap: watching =>
        startService:
          watching.joinWithUnit

  private def watchDirectories: IO[Unit] =
    directoryToFilenames.parFoldMapA: (dir, files) =>
      observeDirectory(dir, files)

  private def directoryToFilenames: Seq[(Path, Set[Path])] =
    files
      .map(path => path.getParent -> path.getFileName)
      .filter: (directory, filename) =>
        directory != null && filename != null
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toVector

  private def observeDirectory(directory: Path, files: Set[Path])
  : IO[Unit] =
    DirectoryStateJvm.readDirectory(directory, files).flatMap: directoryState =>
      DirectoryWatch.stream(directory, directoryState, settings, files)
        .interruptWhenF(untilStopRequested)
        .debounce(settings.directorySilence) // Löscht DirectoryEvents! Sie werden nicht gebraucht
        .tapEachChunk(events => logger.debug(s"HTTPS keys or certificates change signaled: ${
          events.toArraySeq.distinct.mkString(", ")}"))
        .evalTap: _ =>
          onHttpsKeyOrCertChanged
        .compile.drain
    .onErrorTap(t => IO(logger.error(t.toStringWithCauses, t)))

  override def toString = s"HttpsDirectoryWatch(${files.mkString(", ")})"

private object HttpsDirectoryWatch:
  private val logger = Logger[this.type]

  def service(
    settings: DirectoryWatchSettings, files: Seq[Path], onHttpsKeyOrCertChanged: IO[Unit])
  : ResourceIO[HttpsDirectoryWatch] =
    Service.resource:
      new HttpsDirectoryWatch(settings, files, onHttpsKeyOrCertChanged)
