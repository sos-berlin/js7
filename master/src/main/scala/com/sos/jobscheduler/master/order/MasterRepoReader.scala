package com.sos.jobscheduler.master.order

import cats.data.Validated.Valid
import cats.effect.IO
import com.sos.jobscheduler.base.problem.Checked
import com.sos.jobscheduler.core.filebased.{FileBaseds, Repo}
import com.sos.jobscheduler.data.agent.{Agent, AgentPath}
import com.sos.jobscheduler.data.filebased.{FileBased, RepoEvent, TypedPath, VersionId}
import com.sos.jobscheduler.data.workflow.{Workflow, WorkflowId, WorkflowPath}
import com.sos.jobscheduler.master.agent.AgentReader
import com.sos.jobscheduler.master.configuration.MasterConfiguration
import com.sos.jobscheduler.master.workflow.WorkflowReader
import scala.collection.immutable.Seq

/**
  * @author Joacim Zschimmer
  */
protected[order] trait MasterRepoReader extends FileBasedConfigurationReader
{
  protected def masterConfiguration: MasterConfiguration
  protected def onAgentAdded(agent: Agent): Unit
  protected def onConfigurationRead(events: Seq[RepoEvent], repo: Repo, sideEffect: IO[Unit]): Unit

  protected val readers = Set(WorkflowReader, AgentReader)
  protected def directory = masterConfiguration.liveDirectory
  private var _repo = Repo.empty

  final def repo = _repo

  final def readConfiguration(versionId: Option[VersionId]): Checked[IO[Unit]] =
    for (eventsRepoAndSideEffect ← readConfiguration(repo, versionId)) yield
      IO {
        val (events, changedRepo, sideEffect) = eventsRepoAndSideEffect
        onConfigurationRead(events, changedRepo, sideEffect)
      }

  protected def replaceRepo(changed: Repo): Unit =
    _repo = changed

  protected final def updateFileBaseds(diff: FileBaseds.Diff[TypedPath, FileBased]): Seq[Checked[IO[Unit]]] =
    updateAgents(diff.select[AgentPath, Agent])

  private def updateAgents(diff: FileBaseds.Diff[AgentPath, Agent]): Seq[Checked[IO[Unit]]] =
    onlyAdditionPossible(diff) :+
      Valid(IO {
        diff.added foreach onAgentAdded
      })

  final def idToWorkflow(workflowId: WorkflowId): Checked[Workflow] =
    repo.idTo[Workflow](workflowId)

  final def pathToWorkflow: Map[WorkflowPath, Workflow] =
    repo.currentTyped[Workflow]
}
