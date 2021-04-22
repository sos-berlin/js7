package js7.core.item

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.data.item.{ItemPath, VersionedItem, VersionedItemOverview}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait VersionedItemApi
{
  def overview[A <: VersionedItem: VersionedItem.Companion](implicit O: VersionedItemOverview.Companion[A]): Task[Checked[O.Overview]]

  def pathToCurrentItem[A <: VersionedItem: VersionedItem.Companion](path: A#Path): Task[Checked[A]]

  def items[A <: VersionedItem: VersionedItem.Companion]: Task[Checked[Seq[A]]]

  def paths[A <: VersionedItem: VersionedItem.Companion]: Task[Checked[Seq[A#Path]]] =
    for (o <- items) yield
      o.map(_.map(_.path))
}

object VersionedItemApi
{
  def forTest(pathToItem: Map[_ <: ItemPath, VersionedItem]) =
    new VersionedItemApi
    {
      def overview[A <: VersionedItem: VersionedItem.Companion](implicit O: VersionedItemOverview.Companion[A]): Task[Checked[O.Overview]] =
        Task(Right(O.itemsToOverview(pathTo.values.toSeq)))

      def items[A <: VersionedItem: VersionedItem.Companion] =
        Task(Right(pathTo[A].values.toSeq))

      def pathToCurrentItem[A <: VersionedItem: VersionedItem.Companion](path: A#Path) =
        Task(pathTo[A].checked(path))

      def pathTo[A <: VersionedItem] =
        pathToItem.asInstanceOf[Map[A#Path, A]]
    }
}
