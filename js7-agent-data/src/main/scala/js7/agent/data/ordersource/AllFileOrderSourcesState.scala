package js7.agent.data.ordersource

import js7.base.io.file.watch.DirectoryState
import js7.base.problem.Checked
import js7.base.utils.Collections.implicits.RichIterable
import js7.base.utils.ScalaUtils.syntax.RichPartialFunction
import js7.data.event.KeyedEvent
import js7.data.ordersource.{FileOrderSource, OrderSourceEvent, OrderSourceId}
import monix.reactive.Observable
import scala.collection.mutable

final case class AllFileOrderSourcesState(
  idToFileOrderSource: Map[OrderSourceId, FileOrderSourceState])
{
  def estimatedSnapshotSize =
    idToFileOrderSource.values.view.map(_.estimatedSnapshotSize).sum

  def attach(fos: FileOrderSource): AllFileOrderSourcesState =
    copy(
      idToFileOrderSource = idToFileOrderSource +
        (fos.id ->
          (idToFileOrderSource.get(fos.id) match {
            case None => FileOrderSourceState(fos, DirectoryState.empty)
            case Some(fosState) => fosState.copy(fileOrderSource = fos)
          })))

  def applyEvent(keyedEvent: KeyedEvent[OrderSourceEvent]): Checked[AllFileOrderSourcesState] =
    idToFileOrderSource
      .checked(keyedEvent.key)
      .map(o => copy(
        idToFileOrderSource = idToFileOrderSource + (o.id -> o.applyEvent(keyedEvent.event))))

  def toSnapshot: Observable[Any] =
    Observable.fromIterable(idToFileOrderSource.values)
      .flatMap(_.toSnapshot)

  def contains(orderSource: FileOrderSource) =
    idToFileOrderSource.get(orderSource.id)
      .exists(_.fileOrderSource == orderSource)
}

object AllFileOrderSourcesState
{
  val empty = AllFileOrderSourcesState(Map.empty)

  def fromIterable(fileOrderSourceStates: Iterable[FileOrderSourceState]) =
    AllFileOrderSourcesState(fileOrderSourceStates.toKeyedMap(_.id))

  final class Builder
  {
    private val idToFileOrderSource = mutable.Map.empty[OrderSourceId, FileOrderSourceState.Builder]

    def addSnapshot(snapshot: FileOrderSourceState.Snapshot): Unit =
      idToFileOrderSource
        .getOrElseUpdate(snapshot.orderSourceId, new FileOrderSourceState.Builder)
        .addSnapshot(snapshot)

    def result() = AllFileOrderSourcesState(
      idToFileOrderSource.view.mapValues(_.result()).toMap)
  }
}
