package js7.tests.testenv

import cats.effect.unsafe.IORuntime
import fs2.{Pure, Stream}
import java.util.Locale
import js7.base.crypt.Signed
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Atomic, Lazy}
import js7.data.controller.ControllerState
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation.{AddOrChangeOperation, AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.{InventoryItem, InventoryItemPath, ItemOperation, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import scala.annotation.tailrec

trait BlockingItemUpdater:
  private val nextWorkflowNr = Atomic(1)
  private val nextVersionId_ = Atomic(1)

  protected def sign[A <: SignableItem](item: A): Signed[A]
  protected def controller: TestController
  protected def controllerState: ControllerState

  protected final def nextPath[I <: InventoryItemPath](implicit I: InventoryItemPath.Companion[I])
  : I =
    I(I.itemTypeName.toUpperCase(Locale.ROOT) + "-" + nextWorkflowNr.getAndIncrement())

  protected final def nextVersionId() =
    val versionIdSet = controllerState.repo.versionIdSet
    @tailrec def loop(): VersionId =
      val v = VersionId(nextVersionId_.getAndIncrement().toString)
      if versionIdSet contains v then
        loop()
      else
        v
    loop()

  protected final def withTemporaryItem[I <: InventoryItem, A](
    item: I, awaitDeletion: Boolean = false)
    (body: I => A)
    (using IORuntime)
  : A =
    val realItem = updateItem(item)
    var deleted = false
    try
      val a = body(realItem)

      if awaitDeletion && controller.controllerState().keyToItem.contains(realItem.key) then
        controller.eventWatch
          .expect[ItemDeleted](_.event.key == realItem.key):
            deleteItems(realItem.path)
            deleted = true

      a
    finally if !deleted then
      deleteItems(realItem.path)

  protected final def withItems[A](items: InventoryItem*)(body: => A)
    (using IORuntime)
  : A =
    updateItems(items*)
    try body
    finally deleteItems(items.map(_.path)*)

  protected final def updateItem[I <: InventoryItem](item: I)(using IORuntime): I =
    val myItem: I = item match
      case workflow: Workflow if workflow.id.isAnonymous =>
        workflow.withId(nextPath[WorkflowPath]).asInstanceOf[I]
      case o => o
    val v = updateItems(myItem)
    (myItem, v) match
      case (myItem: VersionedItem, Some(v)) => myItem.withVersion(v).asInstanceOf[I]
      case _ => myItem

  protected final def updateItems(items: InventoryItem*)(using IORuntime)
  : Option[VersionId] =
    val versionId = Lazy(nextVersionId())
    val operations: Stream[Pure, AddOrChangeOperation] = Stream
      .iterable:
        items.toVector
          // execute this eagerly to trigger versionId
          .map:
            case item: VersionedItem if item.id.versionId.isAnonymous =>
              item withVersion versionId()
            case o => o
      .map:
        case item: SignableItem => AddOrChangeSigned(sign(item).signedString)
        case item: UnsignedSimpleItem => AddOrChangeSimple(item)

    controller.api
      .updateItems:
        Stream.fromOption(versionId.toOption.map(AddVersion(_)))
          ++ operations
      .await(99.s)
      .orThrow

    versionId.toOption

  protected final def deleteItems(paths: InventoryItemPath*)(using IORuntime): Unit =
    controller.api
      .updateItems:
        Stream
          .fromOption:
            paths.exists(_.isInstanceOf[VersionedItemPath]) ? AddVersion(nextVersionId())
          .append:
            Stream.iterable(paths).map(ItemOperation.Remove(_))
      .await(99.s)
      .orThrow
