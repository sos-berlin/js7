package com.sos.jobscheduler.master

import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedsOverview}
import monix.eval.Task
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
trait FileBasedApi
{
  def overview[A <: FileBased: FileBased.Companion]: Task[Stamped[FileBasedsOverview]]

  def fileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[Stamped[A]]]

  def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A]]]

  def paths[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A#Path]]] =
    for (o ← fileBaseds) yield
      o map (_ map (_.path))
}
