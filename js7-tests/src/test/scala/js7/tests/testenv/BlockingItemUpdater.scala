package js7.tests.testenv

import cats.effect.unsafe.IORuntime
import cats.syntax.functor.*
import fs2.{Pure, Stream}
import js7.base.catsutils.IArrayExtensions.mapOrKeep
import js7.base.crypt.Signed
import js7.base.log.Logger
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.*
import js7.base.utils.{Atomic, Lazy}
import js7.data.item.BasicItemEvent.ItemDeleted
import js7.data.item.ItemOperation.{AddOrChangeOperation, AddOrChangeSigned, AddOrChangeSimple, AddVersion}
import js7.data.item.{InventoryItem, InventoryItemPath, ItemOperation, SignableItem, SimpleItem, UnsignedSimpleItem, VersionId, VersionedItem, VersionedItemPath}
import js7.data.workflow.{Workflow, WorkflowPath}
import js7.tests.testenv.BlockingItemUpdater.*
import org.scalatest.Assertions.assert
import scala.annotation.tailrec
import scala.util.control.NonFatal

trait BlockingItemUpdater:

  private val nextItemNr = Atomic(1)
  private val nextVersionId_ = Atomic(1)
  private val testSuiteName = getClass.simpleScalaName

  protected def sign[A <: SignableItem](item: A): Signed[A]
  protected def controller: TestController

  protected final def nextPath[I <: InventoryItemPath](implicit I: InventoryItemPath.Companion[I])
  : I =
    I(testSuiteName + "-" + nextItemNr.getAndIncrement())
    //I(I.itemTypeName.toUpperCase(Locale.ROOT) + "-" + nextItemNr.getAndIncrement())

  protected final def nextVersionId() =
    val versionIdSet = controller.controllerState().repo.versionIdSet
    @tailrec def loop(): VersionId =
      val v = VersionId(nextVersionId_.getAndIncrement().toString)
      if versionIdSet contains v then
        loop()
      else
        v
    loop()

  protected final def withItem[I <: InventoryItem, A](
    item: I,
    awaitDeletion: Boolean = false)
    (body: I => A)
    (using IORuntime)
  : A =
    withItems(Tuple1(item), awaitDeletion = awaitDeletion): tuple1 =>
      body(tuple1._1)

  protected final def withItems[T <: Tuple, A](
    items: T,
    awaitDeletion: Boolean = false)
    (body: T => A)
    (using IORuntime)
  : A =
    val realItems = updateItemArray:
      items.toIArray.map(_.asInstanceOf[InventoryItem])
    var deleted = false
    try
      val a = body(Tuple.fromIArray(realItems).asInstanceOf[T])
      if awaitDeletion then
        for realItem <- realItems do
          if controller.controllerState().keyToItem.contains(realItem.key) then
            controller.eventWatch.expect[ItemDeleted](_.event.key == realItem.key):
              deleteItems(realItem.path)
            deleted = true
      else
        deleteItems(realItems.map(_.path)*)
      a
    catch case NonFatal(t) if !deleted =>
      try deleteItems(realItems.map(_.path)*)
      catch case NonFatal(tt) =>
        if tt ne t then t.addSuppressed(tt)
        if t.getSuppressed.isEmpty then logger.error(s"deleteItems => ${t.toStringWithCauses}")
      throw t

  protected final def updateItem[I <: InventoryItem](item: I)(using IORuntime): I =
    updateItemArray(IArray[InventoryItem](item)).head.asInstanceOf[I]

  private def updateItemArray(items: IArray[InventoryItem])(using IORuntime): IArray[InventoryItem] =
    val namedItems = items.mapOrKeep:
      case workflow: Workflow if workflow.id.isAnonymous =>
        workflow.withId(nextPath[WorkflowPath])

    val v = updateItems(namedItems*)

    // Add VersionId and ItemRevision to items
    namedItems.map: myItem =>
      var item = myItem
      item = (item, v) match
        case (item: VersionedItem, Some(v)) => item.withVersion(v)
        case _ => item
      val controllerItem = controller.controllerState().keyToItem(item.key)
      item = item match
        case item: SimpleItem => item.withRevision(controllerItem.itemRevision)
        case o => o
      assert(item == controllerItem)
      item

  protected final def updateItems(items: InventoryItem*)(using IORuntime): Option[VersionId] =
    val versionId = Lazy(nextVersionId())
    val operations: Stream[Pure, AddOrChangeOperation] = Stream
      .iterable:
        // execute this eagerly to trigger versionId
        items.toVector.mapOrKeep:
          case item: VersionedItem if item.id.versionId.isAnonymous =>
            item.withVersion(versionId())
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


object BlockingItemUpdater:
  private lazy val logger = Logger[this.type]
