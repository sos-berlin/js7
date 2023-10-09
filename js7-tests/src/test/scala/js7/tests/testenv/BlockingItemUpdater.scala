package js7.tests.testenv

import java.util.Locale
import js7.base.crypt.Signed
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Atomic, Lazy}
import js7.data.controller.ControllerState
import js7.data.item.ItemOperation.{AddOrChangeOperation, AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.{InventoryItem, InventoryItemPath, ItemOperation, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import monix.execution.Scheduler
import monix.reactive.Observable
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

  protected final def withTemporaryItem[I <: InventoryItem, A](item: I)(body: I => A)
    (implicit s: Scheduler)
  : A =
    val realItem = updateItem(item)
    try body(realItem)
    finally deleteItems(realItem.path)

  protected final def withItems[A](items: InventoryItem*)(body: => A)
    (implicit s: Scheduler)
  : A =
    updateItems(items*)
    try body
    finally deleteItems(items.map(_.path)*)

  protected final def updateItem[I <: InventoryItem](item: I)(implicit s: Scheduler): I =
    val myItem: I = item match
      case workflow: Workflow if workflow.id.isAnonymous =>
        workflow.withId(nextPath[WorkflowPath]).asInstanceOf[I]
      case o => o
    val v = updateItems(myItem)
    (myItem, v) match
      case (myItem: VersionedItem, Some(v)) => myItem.withVersion(v).asInstanceOf[I]
      case _ => myItem

  protected final def updateItems(items: InventoryItem*)(implicit s: Scheduler)
  : Option[VersionId] =
    val versionId = Lazy(nextVersionId())
    val operations: Vector[AddOrChangeOperation] = items
      .toVector
      .map:
        case item: VersionedItem if item.id.versionId.isAnonymous =>
          // Update versionId as a side effect !
          item withVersion versionId()
        case o => o
      .map:
        case item: SignableItem => AddOrChangeSigned(sign(item).signedString)
        case item: UnsignedSimpleItem => AddOrChangeSimple(item)

    controller.api
      .updateItems(
        Observable.fromIterable(versionId.toOption.map(AddVersion(_))) ++
          Observable.fromIterable(operations))
      .await(99.s)
      .orThrow

    versionId.toOption

  protected final def deleteItems(paths: InventoryItemPath*)(implicit s: Scheduler): Unit =
    controller.api
      .updateItems(
        Observable.fromIterable(
          (paths.exists(_.isInstanceOf[VersionedItemPath]) ? AddVersion(nextVersionId())) ++
            paths.map(ItemOperation.Remove(_))))
      .await(99.s)
      .orThrow
