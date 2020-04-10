package com.sos.jobscheduler.core.filebased

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedsOverview, TypedPath}
import monix.eval.Task

/**
  * @author Joacim Zschimmer
  */
trait FileBasedApi
{
  def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Stamped[O.Overview]]

  def idTo[A <: FileBased: FileBased.Companion](id: A#Id): Task[Stamped[Checked[A]]]

  def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[Stamped[A]]]

  def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A]]]

  def paths[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A#Path]]] =
    for (o <- fileBaseds) yield
      o map (_ map (_.path))
}

object FileBasedApi {
  def forTest(pathToFileBased: Map[_ <: TypedPath, FileBased]) =
    new FileBasedApi {
      def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Stamped[O.Overview]] =
        Task(Stamped(1, O.fileBasedsToOverview(pathTo.values.toImmutableSeq)))

      def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
        throw new NotImplementedError

      def fileBaseds[A <: FileBased: FileBased.Companion] =
        Task(Stamped(2, pathTo[A].values.toImmutableSeq))

      def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path) =
        Task(
          for (a <- pathTo[A].checked(path))
            yield Stamped(3, a))

      def pathTo[A <: FileBased] = pathToFileBased.asInstanceOf[Map[A#Path, A]]
    }
}
