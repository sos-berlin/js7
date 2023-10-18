package js7.common.pekkohttp.web

import cats.effect.Resource
import cats.syntax.all.*
import java.nio.file.Path
import js7.base.io.file.watch.{DirectoryStateJvm, DirectoryWatch, DirectoryWatchSettings}
import js7.base.log.Logger
import js7.base.monixutils.MonixBase.syntax.RichMonixObservable
import js7.base.service.Service
import js7.base.thread.IOExecutor
import js7.base.utils.ScalaUtils.syntax.RichThrowable
import js7.common.pekkohttp.web.HttpsDirectoryWatch.*
import monix.eval.Task
import monix.reactive.Observable

private final class HttpsDirectoryWatch private(
  settings: DirectoryWatchSettings,
  files: Seq[Path],
  onHttpsKeyOrCertChanged: Task[Unit])
  (implicit iox: IOExecutor)
extends Service.StoppableByRequest:

  protected def start =
    watchDirectories
      .start
      .flatMap(watching =>
        startService(
          watching.join))

  private def watchDirectories: Task[Unit] =
    directoryToFilenames
      .parTraverse { case (dir, files) => observeDirectory(dir, files) }
      .map(_.combineAll)

  private def directoryToFilenames: Seq[(Path, Set[Path])] =
    files
      .map(path => path.getParent -> path.getFileName)
      .filter { case (directory, filename) => directory != null && filename != null }
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.toSet)
      .toVector

  private def observeDirectory(directory: Path, files: Set[Path])(implicit iox: IOExecutor)
  : Task[Unit] =
    Task
      .defer:
        val directoryState = DirectoryStateJvm.readDirectory(directory, files) // throws
        DirectoryWatch
          .observable(
            directory, directoryState, settings, files)
          .takeUntilEval(untilStopRequested)
          .flatMap(Observable.fromIterable)
          .debounce(settings.directorySilence) // Löscht DirectoryEvents! Sie werden nicht gebraucht
          .bufferIntrospective(1024)
          .tapEach(events => logger.debug(
            s"HTTPS keys or certificates change signaled: ${events.distinct.mkString(", ")}"))
          .tapEval(_ =>
            onHttpsKeyOrCertChanged)
          .completedL
      .tapError(t => Task(logger.error(t.toStringWithCauses, t)))

private object HttpsDirectoryWatch:
  def resource(
    settings: DirectoryWatchSettings, files: Seq[Path], onHttpsKeyOrCertChanged: Task[Unit])
    (implicit iox: IOExecutor)
  : Resource[Task, HttpsDirectoryWatch] =
    Service.resource(Task(new HttpsDirectoryWatch(settings, files, onHttpsKeyOrCertChanged)))

  private val logger = Logger[this.type]
