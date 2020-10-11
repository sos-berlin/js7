package js7.core.item

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.{InventoryItem, InventoryItemOverview, ItemPath}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait InventoryItemApi
{
  def overview[A <: InventoryItem: InventoryItem.Companion](implicit O: InventoryItemOverview.Companion[A]): Task[Checked[O.Overview]]

  def idTo[A <: InventoryItem: InventoryItem.Companion](id: A#Id): Task[Checked[A]]

  def pathToCurrentItem[A <: InventoryItem: InventoryItem.Companion](path: A#Path): Task[Checked[A]]

  def items[A <: InventoryItem: InventoryItem.Companion]: Task[Checked[Seq[A]]]

  def paths[A <: InventoryItem: InventoryItem.Companion]: Task[Checked[Seq[A#Path]]] =
    for (o <- items) yield
      o map (_ map (_.path))
}

object InventoryItemApi
{
  def forTest(pathToItem: Map[_ <: ItemPath, InventoryItem]) =
    new InventoryItemApi
    {
      def overview[A <: InventoryItem: InventoryItem.Companion](implicit O: InventoryItemOverview.Companion[A]): Task[Checked[O.Overview]] =
        Task(Right(O.itemsToOverview(pathTo.values.toSeq)))

      def idTo[A <: InventoryItem: InventoryItem.Companion](id: A#Id) =
        throw new NotImplementedError

      def items[A <: InventoryItem: InventoryItem.Companion] =
        Task(Right(pathTo[A].values.toSeq))

      def pathToCurrentItem[A <: InventoryItem: InventoryItem.Companion](path: A#Path) =
        Task(pathTo[A].checked(path))

      def pathTo[A <: InventoryItem] =
        pathToItem.asInstanceOf[Map[A#Path, A]]
    }
}
