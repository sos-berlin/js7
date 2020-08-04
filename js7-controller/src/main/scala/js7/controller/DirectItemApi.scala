package js7.controller

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.controller.data.ControllerState
import js7.core.item.InventoryItemApi
import js7.data.item.{InventoryItem, InventoryItemOverview, ItemId, Repo, TypedPath}
import monix.eval.Task

private[controller] final class DirectItemApi(controllerState: Task[Checked[ControllerState]])
extends InventoryItemApi
{
  def overview[A <: InventoryItem: InventoryItem.Companion](implicit O: InventoryItemOverview.Companion[A]): Task[Checked[O.Overview]] =
    for (checked <- checkedRepo) yield
      for (repo <- checked) yield
        O.itemsToOverview(repo.currentTyped[A].values.toSeq)

  def idTo[A <: InventoryItem: InventoryItem.Companion](id: A#Id) =
    for (checked <- checkedRepo) yield
      checked.flatMap(_.idTo[A](id))

  def items[A <: InventoryItem: InventoryItem.Companion]: Task[Checked[Seq[A]]] =
    for (checked <- checkedRepo) yield
      for (repo <- checked) yield
        repo.currentTyped[A].values.toSeq.sortBy/*for determinstic tests*/(_.id: ItemId[TypedPath])

  def pathToCurrentItem[A <: InventoryItem: InventoryItem.Companion](path: A#Path): Task[Checked[A]] =
    for (checked <- checkedRepo) yield
    checked.flatMap(_.currentTyped[A].checked(path))

  def checkedRepo: Task[Checked[Repo]] =
    controllerState.map(_.map(_.repo))
}
