package com.sos.jobscheduler.master

import akka.actor.ActorRef
import akka.pattern.ask
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.base.utils.Collections.implicits.RichTraversableOnce
import com.sos.jobscheduler.base.utils.IntelliJUtils.intelliJuseImport
import com.sos.jobscheduler.base.utils.ScalaUtils.RichPartialFunction
import com.sos.jobscheduler.core.filebased.{FileBasedApi, Repo}
import com.sos.jobscheduler.data.event.Stamped
import com.sos.jobscheduler.data.filebased.{FileBased, FileBasedId, FileBasedsOverview, TypedPath}
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import monix.eval.Task
import scala.collection.immutable.Seq
import shapeless.tag.@@

private[master] final class MainFileBasedApi(
  masterConfiguration: MasterConfiguration,
  orderKeeper: ActorRef @@ MasterOrderKeeper.type)
extends FileBasedApi
{
  def overview[A <: FileBased: FileBased.Companion](implicit O: FileBasedsOverview.Companion[A]): Task[Stamped[O.Overview]] =
    for (stamped <- stampedRepo) yield
      for (repo <- stamped) yield
        O.fileBasedsToOverview(repo.currentTyped[A].values.toImmutableSeq)

  def idTo[A <: FileBased: FileBased.Companion](id: A#Id) =
    for (stamped <- stampedRepo) yield
      for (repo <- stamped) yield
        repo.idTo[A](id)

  def fileBaseds[A <: FileBased: FileBased.Companion]: Task[Stamped[Seq[A]]] =
    for (stamped <- stampedRepo) yield
      for (repo <- stamped) yield
        repo.currentTyped[A].values.toImmutableSeq.sortBy/*for determinstic tests*/(_.id: FileBasedId[TypedPath])

  def pathToCurrentFileBased[A <: FileBased: FileBased.Companion](path: A#Path): Task[Checked[Stamped[A]]] =
    for (stamped <- stampedRepo; repo = stamped.value) yield
      for (a <- repo.currentTyped[A].checked(path)) yield
        stamped.copy(value = a)

  def stampedRepo: Task[Stamped[Repo]] = {
    import masterConfiguration.akkaAskTimeout  // TODO May timeout while Master recovers
    intelliJuseImport(akkaAskTimeout)
    Task.deferFuture(
      (orderKeeper ? MasterOrderKeeper.Command.GetRepo).mapTo[Stamped[Repo]])
  }
}
