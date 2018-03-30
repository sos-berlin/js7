package com.sos.jobscheduler.master.fileBased

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedsOverview}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait FileBasedApi
{
  def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Stamped[O.Overview]]

  def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[Stamped[A]]]

  def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A]]]

  def paths[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A#Path]]] =
    for (o ← fileBaseds) yield
      o map (_ map (_.path))
}

object FileBasedApi {
  def forTest[T <: FileBased: FileBased.Companion](pathToFileBased: Map[T#Path, T]) =
    new FileBasedApi {
      def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Stamped[O.Overview]] =
        Task.now(Stamped(1, O.fileBasedsToOverview(pathTo.values.toImmutableSeq)))

      def fileBaseds[A <: FileBased: FileBased.Companion] =
        Task.now(Stamped(2, pathTo[A].values.toImmutableSeq))

      def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path) =
        Task.now(
          for (a ← pathTo[A].checked(path))
            yield Stamped(3, a))

      def pathTo[A <: FileBased] = pathToFileBased.asInstanceOf[Map[A#Path, A]]
    }
}
