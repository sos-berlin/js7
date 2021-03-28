package js7.controller

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.syntax._
import js7.core.item.VersionedItemApi
import js7.data.controller.ControllerState
import js7.data.item.{ItemPath, Repo, VersionedItem, VersionedItemId, VersionedItemOverview}
import monix.eval.Task

private[controller] final class DirectItemApi(controllerState: Task[Checked[ControllerState]])
extends VersionedItemApi
{
  def overview[A <: VersionedItem: VersionedItem.Companion](implicit O: VersionedItemOverview.Companion[A]): Task[Checked[O.Overview]] =
    for (checked <- checkedRepo) yield
      for (repo <- checked) yield
        O.itemsToOverview(repo.currentTyped[A].values.toSeq)

  //def idTo[A <: VersionedItem: VersionedItem.Companion](id: A#Id) =
  //  for (checked <- checkedRepo) yield
  //    checked.flatMap(_.idTo[A](id))

  def items[A <: VersionedItem: VersionedItem.Companion]: Task[Checked[Seq[A]]] =
    for (checked <- checkedRepo) yield
      for (repo <- checked) yield
        repo.currentTyped[A].values.toSeq
          .sortBy/*for determinstic tests*/(_.id: VersionedItemId[ItemPath])

  def pathToCurrentItem[A <: VersionedItem: VersionedItem.Companion](path: A#Path): Task[Checked[A]] =
    for (checked <- checkedRepo) yield
    checked.flatMap(_.currentTyped[A].checked(path))

  def checkedRepo: Task[Checked[Repo]] =
    controllerState.map(_.map(_.repo))
}
