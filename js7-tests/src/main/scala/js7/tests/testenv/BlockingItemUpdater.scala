package js7.tests.testenv

import js7.base.crypt.Signed
import js7.base.thread.MonixBlocking.syntax._
import js7.base.time.ScalaTime._
import js7.base.utils.Lazy
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.ItemOperation.{AddOrChangeOperation, AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.{InventoryItem, SignableItem, UnsignedSimpleItem, VersionId, VersionedItem}
import js7.proxy.ControllerApi
import monix.execution.Scheduler
import monix.execution.atomic.Atomic
import monix.reactive.Observable

trait BlockingItemUpdater {
  private val nextVersionId = Atomic(1)

  protected def sign[A <: SignableItem](item: A): Signed[A]
  protected def controllerApi: ControllerApi

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
    val versionId = Lazy(VersionId(nextVersionId.getAndIncrement().toString))
    val operations: Vector[AddOrChangeOperation] = items
      .toVector
      .map {
        case item: VersionedItem if item.id.versionId.isAnonymous => item withVersion versionId()
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
}
