package js7.data.controller

import cats.syntax.traverse._
import js7.base.problem.Problems.DuplicateKey
import js7.base.problem.{Checked, Problem}
import js7.base.utils.ScalaUtils.syntax.{RichBoolean, RichEither}
import js7.data.controller.ControllerStateExecutor.convertImplicitly
import js7.data.crypt.SignedItemVerifier
import js7.data.event.KeyedEvent.NoKey
import js7.data.event.{KeyedEvent, NoKeyEvent}
import js7.data.item.BasicItemEvent.{ItemDestroyed, ItemDestructionMarked}
import js7.data.item.SignedItemEvent.{SignedItemAdded, SignedItemChanged}
import js7.data.item.UnsignedSimpleItemEvent.{UnsignedSimpleItemAdded, UnsignedSimpleItemAddedOrChanged, UnsignedSimpleItemChanged}
import js7.data.item.{BasicItemEvent, InventoryItemEvent, ItemRevision, SignableSimpleItem, SimpleItemPath, UnsignedSimpleItem, VersionedItemPath}
import js7.data.orderwatch.OrderWatchPath
import scala.collection.View

trait VerifiedUpdateItemsExecutor
{
  this: ControllerStateExecutor =>

  def executeVerifiedUpdateItems(verifiedUpdateItems: VerifiedUpdateItems)
  : Checked[Seq[KeyedEvent[NoKeyEvent]]] =
    verifiedUpdateItems.maybeVersioned
      .fold[Checked[Seq[NoKeyEvent]]](Right(Nil))(verifiedUpdateItemsVersionedToEvents)
      .flatMap { versionedEvents =>
        val previous = controllerState
        for {
          controllerState <- controllerState.applyEvents(versionedEvents.map(NoKey <-: _))
          simpleItemEvents <- controllerState.verifiedUpdateItemSimpleToEvents(verifiedUpdateItems.simple)
          controllerState <- controllerState.applyEvents(simpleItemEvents.map(NoKey <-: _))
          _ <- controllerState.checkVerifiedUpdateConsistency(verifiedUpdateItems, previous)
        } yield (simpleItemEvents ++ versionedEvents).map(NoKey <-: _).toVector
      }
      .left.map {
        case prblm @ Problem.Combined(Seq(_, duplicateKey: DuplicateKey)) =>
          scribe.debug(prblm.toString)
          duplicateKey
        case o => o
      }

  private def verifiedUpdateItemsVersionedToEvents(versioned: VerifiedUpdateItems.Versioned)
  : Checked[Seq[NoKeyEvent]] =
    controllerState.repo
      .itemsToEventBlock(
        versioned.versionId,
        versioned.verifiedItems.map(_.signedItem),
        versioned.delete)
      .map(block =>
        block.events ++
          block.deletedEvents.view.map(_.path).flatMap(destroyVersionedItem))

  private def destroyVersionedItem(path: VersionedItemPath): Option[ItemDestroyed] =
    controllerState.repo
      .pathToItem(path).toOption
      .filter(item => !controllerState.isCurrentOrStillInUse(item.id))
      .flatMap(item =>
        !controllerState.itemToAgentToAttachedState.contains(item.id) ?
          ItemDestroyed(item.id))

  protected final def verifiedUpdateItemSimpleToEvents(simple: VerifiedUpdateItems.Simple)
  : Checked[View[InventoryItemEvent]] =
    simple.verifiedSimpleItems
      .traverse(updateVerifiedSimpleItemToEvents)
      .flatMap(signedEvents =>
        simple.unsignedSimpleItems
          .traverse(unsignedSimpleItemToEvent)
          .map(unsignedEvents =>
            simple.delete.view
              .flatMap(simpleItemDestroyEvents)
              .view ++ signedEvents ++ unsignedEvents))

  private def updateVerifiedSimpleItemToEvents(verified: SignedItemVerifier.Verified[SignableSimpleItem])
  : Checked[InventoryItemEvent] = {
    val item = verified.item
    if (item.itemRevision.isDefined)
      Left(Problem.pure("ItemRevision is not accepted here"))
    else
      Right(
        controllerState.pathToSimpleItem.get(item.key) match {
          case None =>
            SignedItemAdded(verified.signedItem.copy(value =
              item.withRevision(Some(ItemRevision.Initial))))
          case Some(existing) =>
            SignedItemChanged(verified.signedItem.copy(
              value = verified.signedItem.value
                .withRevision(Some(
                  existing.itemRevision.fold(ItemRevision.Initial/*not expected*/)(_.next)))))
        })
  }

  private def unsignedSimpleItemToEvent(item: UnsignedSimpleItem)
  : Checked[UnsignedSimpleItemAddedOrChanged] =
    if (item.itemRevision.isDefined)
      Left(Problem.pure("ItemRevision is not accepted here"))
    else
      Right(
        controllerState.pathToSimpleItem.get(item.key) match {
          case None =>
            UnsignedSimpleItemAdded(item.withRevision(Some(ItemRevision.Initial)))
          case Some(existing) =>
            UnsignedSimpleItemChanged(item
              .withRevision(Some(
                existing.itemRevision.fold(ItemRevision.Initial/*not expected*/)(_.next))))
        })

  private def simpleItemDestroyEvents(path: SimpleItemPath): View[BasicItemEvent] =
    path match {
      case path: OrderWatchPath =>
        if (controllerState.itemToAgentToAttachedState.contains(path))
          (!controllerState.destructionMarkedItems.contains(path) ? ItemDestructionMarked(path)).view ++
            controllerState.detach(path)
        else
          View(ItemDestroyed(path))

      case _ =>
        View(ItemDestroyed(path))
    }

  protected final def checkVerifiedUpdateConsistency(
    verifiedUpdateItems: VerifiedUpdateItems,
    previous: ControllerState
  ): Checked[Unit] = {
    val newChecked = controllerState.checkConsistencyForItems(verifiedUpdateItems.addOrChangeKeys)
    val delChecked = controllerState.checkConsistencyForDeletedItems(
      verifiedUpdateItems.simple.delete.view ++
        verifiedUpdateItems.maybeVersioned.view
          .flatMap(_.delete)
          .flatMap(previous.repo.pathToId))
    newChecked.combineLeftOrRight(delChecked)
  }
}
