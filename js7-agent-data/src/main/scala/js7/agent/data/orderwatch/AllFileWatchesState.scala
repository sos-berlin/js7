package js7.agent.data.orderwatch

import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.event.KeyedEvent
import js7.data.orderwatch.{FileWatch, OrderWatchEvent, OrderWatchPath}
import monix.reactive.Observable
import scala.collection.mutable

final case class AllFileWatchesState(
  pathToFileWatchState: Map[OrderWatchPath, FileWatchState])
{
  def estimatedSnapshotSize =
    pathToFileWatchState.values.view.map(_.estimatedSnapshotSize).sum

  def attach(fileWatch: FileWatch): AllFileWatchesState =
    copy(
      pathToFileWatchState = pathToFileWatchState +
        (fileWatch.id ->
          (pathToFileWatchState.get(fileWatch.id) match {
            case None => FileWatchState(fileWatch, DirectoryState.empty)
            case Some(fileWatchState) => fileWatchState.copy(fileWatch = fileWatch)
          })))

  def detach(orderWatchPath: OrderWatchPath): AllFileWatchesState =
    copy(
      pathToFileWatchState = pathToFileWatchState - orderWatchPath)

  def applyEvent(keyedEvent: KeyedEvent[OrderWatchEvent]): Checked[AllFileWatchesState] =
    pathToFileWatchState
      .checked(keyedEvent.key)
      .map(o => copy(
        pathToFileWatchState = pathToFileWatchState + (o.id -> o.applyEvent(keyedEvent.event))))

  def toSnapshot: Observable[Any] =
    Observable.fromIterable(pathToFileWatchState.values)
      .flatMap(_.toSnapshot)

  def contains(fileWatch: FileWatch) =
    pathToFileWatchState.get(fileWatch.id)
      .exists(_.fileWatch == fileWatch)
}

object AllFileWatchesState
{
  val empty = AllFileWatchesState(Map.empty)

  def fromIterable(fileWatchStates: Iterable[FileWatchState]) =
    AllFileWatchesState(fileWatchStates.toKeyedMap(_.id))

  final class Builder
  {
    private val pathToFileWatch = mutable.Map.empty[OrderWatchPath, FileWatchState.Builder]

    def addSnapshot(snapshot: FileWatchState.Snapshot): Unit =
      pathToFileWatch
        .getOrElseUpdate(snapshot.orderWatchPath, new FileWatchState.Builder)
        .addSnapshot(snapshot)

    def result() = AllFileWatchesState(
      pathToFileWatch.view.mapValues(_.result()).toMap)
  }
}
