package js7.tests.testenv

import js7.base.crypt.Signed
import js7.base.thread.MonixBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax.*
import js7.data.controller.ControllerState
import js7.data.item.ItemOperation.{AddOrChangeOperation, AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.{InventoryItem, InventoryItemPath, ItemOperation, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.proxy.ControllerApi
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import monix.reactive.Observable
import scala.annotation.tailrec

trait BlockingItemUpdater {
  private val nextVersionId_ = Atomic(1)

  protected def sign[A <: SignableItem](item: A): Signed[A]
  protected def controllerApi: ControllerApi
  protected def controllerState: ControllerState

  protected final def nextVersionId() = {
    val versionIdSet = controllerState.repo.versionIdSet
    @tailrec def loop(): VersionId = {
      val v = VersionId(nextVersionId_.getAndIncrement().toString)
      if (versionIdSet contains v)
        loop()
      else
        v
    }
    loop()
  }

  protected final def withTemporaryItem[I <: InventoryItem, A](item: I)(body: I => A)
    (implicit s: Scheduler)
  : A = {
    val realItem = updateItem(item)
    try body(realItem)
    finally deleteItems(realItem.path)
  }

  protected final def withItems[A](items: InventoryItem*)(body: => A)
    (implicit s: Scheduler)
  : A = {
    updateItems(items*)
    try body
    finally deleteItems(items.map(_.path)*)
  }

  protected final def updateItem[I <: InventoryItem](item: I)(implicit s: Scheduler)
  : I = {
    val v = updateItems(item)
    (item, v) match {
      case (item: VersionedItem, Some(v)) => item.withVersion(v).asInstanceOf[I]
      case _ => item
    }
  }

  protected final def updateItems(items: InventoryItem*)(implicit s: Scheduler)
  : Option[VersionId] = {
    val versionId = Lazy(nextVersionId())
    val operations: Vector[AddOrChangeOperation] = items
      .toVector
      .map {
        case item: VersionedItem if item.id.versionId.isAnonymous =>
          // Update versionId as a side effect !
          item withVersion versionId()
        case o => o
      }
      .map {
        case item: SignableItem => AddOrChangeSigned(sign(item).signedString)
        case item: UnsignedSimpleItem => AddOrChangeSimple(item)
      }

    controllerApi
      .updateItems(
        Observable.fromIterable(versionId.toOption.map(AddVersion(_))) ++
          Observable.fromIterable(operations))
      .await(99.s)
      .orThrow

    versionId.toOption
  }

  protected final def deleteItems(paths: InventoryItemPath*)(implicit s: Scheduler): Unit =
    controllerApi
      .updateItems(
        Observable.fromIterable(
          (paths.exists(_.isInstanceOf[VersionedItemPath]) ? AddVersion(nextVersionId())) ++
            paths.map(ItemOperation.Remove(_))))
      .await(99.s)
      .orThrow
}
