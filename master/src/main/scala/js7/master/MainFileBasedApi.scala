package js7.master

import js7.base.problem.Checked
import js7.base.utils.ScalaUtils.RichPartialFunction
import js7.core.filebased.FileBasedApi
import js7.data.filebased.{FileBased, FileBasedId, FileBasedsOverview, Repo, TypedPath}
import js7.master.data.MasterState
import monix.eval.Task

private[master] final class MainFileBasedApi(masterState: Task[Checked[MasterState]])
extends FileBasedApi
{
  def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Checked[O.Overview]] =
    for (checked <- checkedRepo) yield
      for (repo <- checked) yield
        O.fileBasedsToOverview(repo.currentTyped[A].values.toSeq)

  def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
    for (checked <- checkedRepo) yield
      checked.flatMap(_.idTo[A](id))

  def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Checked[Seq[A]]] =
    for (checked <- checkedRepo) yield
      for (repo <- checked) yield
        repo.currentTyped[A].values.toSeq.sortBy/*for determinstic tests*/(_.id: FileBasedId[TypedPath])

  def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[A]] =
    for (checked <- checkedRepo) yield
    checked.flatMap(_.currentTyped[A].checked(path))

  def checkedRepo: Task[Checked[Repo]] =
    masterState.map(_.map(_.repo))
}
