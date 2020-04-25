package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedsOverview, TypedPath}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait FileBasedApi
{
  def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Checked[O.Overview]]

  def idTo[A <: FileBased: FileBased.Companion](id: A#Id): Task[Checked[A]]

  def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[A]]

  def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Checked[Seq[A]]]

  def paths[A <: FileBased: FileBased.Companion]: Task[Checked[Seq[A#Path]]] =
    for (o <- fileBaseds) yield
      o map (_ map (_.path))
}

object FileBasedApi
{
  def forTest(pathToFileBased: Map[_ <: TypedPath, FileBased]) =
    new FileBasedApi
    {
      def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Checked[O.Overview]] =
        Task(Right(O.fileBasedsToOverview(pathTo.values.toSeq)))

      def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
        throw new NotImplementedError

      def fileBaseds[A <: FileBased: FileBased.Companion] =
        Task(Right(pathTo[A].values.toSeq))

      def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path) =
        Task(pathTo[A].checked(path))

      def pathTo[A <: FileBased] =
        pathToFileBased.asInstanceOf[Map[A#Path, A]]
    }
}
