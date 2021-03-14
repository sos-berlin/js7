package js7.agent.data.orderwatch

import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.event.KeyedEvent
import js7.data.orderwatch.{FileWatch, OrderWatchEvent, OrderWatchId}
import monix.reactive.Observable
import scala.collection.mutable

final case class AllFileWatchesState(
  idToFileWatch: Map[OrderWatchId, FileWatchState])
{
  def estimatedSnapshotSize =
    idToFileWatch.values.view.map(_.estimatedSnapshotSize).sum

  def attach(fos: FileWatch): AllFileWatchesState =
    copy(
      idToFileWatch = idToFileWatch +
        (fos.id ->
          (idToFileWatch.get(fos.id) match {
            case None => FileWatchState(fos, DirectoryState.empty)
            case Some(fosState) => fosState.copy(fileWatch = fos)
          })))

  def applyEvent(keyedEvent: KeyedEvent[OrderWatchEvent]): Checked[AllFileWatchesState] =
    idToFileWatch
      .checked(keyedEvent.key)
      .map(o => copy(
        idToFileWatch = idToFileWatch + (o.id -> o.applyEvent(keyedEvent.event))))

  def toSnapshot: Observable[Any] =
    Observable.fromIterable(idToFileWatch.values)
      .flatMap(_.toSnapshot)

  def contains(orderWatch: FileWatch) =
    idToFileWatch.get(orderWatch.id)
      .exists(_.fileWatch == orderWatch)
}

object AllFileWatchesState
{
  val empty = AllFileWatchesState(Map.empty)

  def fromIterable(fileWatchStates: Iterable[FileWatchState]) =
    AllFileWatchesState(fileWatchStates.toKeyedMap(_.id))

  final class Builder
  {
    private val idToFileWatch = mutable.Map.empty[OrderWatchId, FileWatchState.Builder]

    def addSnapshot(snapshot: FileWatchState.Snapshot): Unit =
      idToFileWatch
        .getOrElseUpdate(snapshot.orderWatchId, new FileWatchState.Builder)
        .addSnapshot(snapshot)

    def result() = AllFileWatchesState(
      idToFileWatch.view.mapValues(_.result()).toMap)
  }
}
