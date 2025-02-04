package js7.agent.data.orderwatch

import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.event.KeyedEvent
import js7.data.orderwatch.{FileWatch, OrderWatchEvent, OrderWatchPath}
import scala.collection.{MapView, mutable}

trait FileWatchStateHandler[Self]:
  self: Self =>

  protected def pathToFileWatchState: MapView[OrderWatchPath, FileWatchState]

  protected def updateFileWatchStates(
    fileWatchStates: Seq[FileWatchState] = Nil,
    remove: Seq[OrderWatchPath] = Nil)
  : Checked[Self]

  private def updateFileWatchState(fileWatchState: FileWatchState): Checked[Self] =
    updateFileWatchStates(fileWatchState :: Nil)

  /** FileWatch handler. */
  object fw:
    def estimatedExtraSnapshotSize: Int =
      pathToFileWatchState.values.view.map(_.estimatedExtraSnapshotSize).sum

    def attach(fileWatch: FileWatch): Checked[Self] =
      updateFileWatchState(
        pathToFileWatchState.get(fileWatch.path) match {
          case None => FileWatchState(fileWatch, DirectoryState.empty)
          case Some(fileWatchState) => fileWatchState.copy(fileWatch = fileWatch)
        })

    def detach(path: OrderWatchPath): Checked[Self] =
      pathToFileWatchState.checked(path)
        .flatMap(_ => updateFileWatchStates(remove = path :: Nil))

    def applyOrderWatchEvent(keyedEvent: KeyedEvent[OrderWatchEvent]): Checked[Self] =
      pathToFileWatchState
        .checked(keyedEvent.key)
        .flatMap:
          _.applyEvent(keyedEvent.event)
        .flatMap: o =>
          updateFileWatchState(o)


object FileWatchStateHandler:

  final class Builder:
    private val pathToFileWatch = mutable.Map.empty[OrderWatchPath, FileWatchState.Builder]

    def addSnapshot(snapshot: FileWatchState.Snapshot): Unit =
      pathToFileWatch
        .getOrElseUpdate(snapshot.orderWatchPath, new FileWatchState.Builder)
        .addSnapshot(snapshot)

    def result: Map[OrderWatchPath, FileWatchState] =
      pathToFileWatch.view.mapValues(_.result()).toMap
