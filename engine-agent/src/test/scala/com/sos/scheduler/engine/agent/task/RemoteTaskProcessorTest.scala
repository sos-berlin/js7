package com.sos.scheduler.engine.agent.task

import com.google.inject.{AbstractModule, Guice, Provides}
import com.sos.scheduler.engine.agent.commands.{CloseRemoteTask, CloseRemoteTaskResponse, StartRemoteTask, StartRemoteTaskResponse}
import com.sos.scheduler.engine.agent.task.RemoteTaskProcessorTest._
import com.sos.scheduler.engine.common.guice.GuiceImplicits._
import com.sos.scheduler.engine.data.agent.RemoteTaskId
import com.sos.scheduler.engine.taskserver.task.StartConfiguration
import java.net.InetSocketAddress
import javax.inject.Singleton
import org.junit.runner.RunWith
import org.mockito.Mockito._
import org.scalatest.FreeSpec
import org.scalatest.Inside.inside
import org.scalatest.Matchers._
import org.scalatest.concurrent.AsyncAssertions.Waiter
import org.scalatest.junit.JUnitRunner
import org.scalatest.mock.MockitoSugar.mock
import org.scalatest.time.SpanSugar._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Try}

/**
 * @author Joacim Zschimmer
 */
@RunWith(classOf[JUnitRunner])
final class RemoteTaskProcessorTest extends FreeSpec {
  private lazy val remoteTasks = List.fill(2) { mock[RemoteTask] }
  private lazy val remoteTaskHandler = Guice.createInjector(new TestModule(remoteTasks)).apply[RemoteTaskProcessor]

  "StartRemoteTask" in {
    val command = StartRemoteTask(new InetSocketAddress("127.0.0.1", 9999), usesApi = false, javaOptions = JavaOptions, javaClassPath = JavaClasspath)
    for (nextRemoteTaskId ← RemoteTaskIds) {
      val response = Await.result(remoteTaskHandler.executeCommand(command), 1.seconds)
      inside(response) { case StartRemoteTaskResponse(id) ⇒ id shouldEqual nextRemoteTaskId }
    }
    for (remoteTask ← remoteTasks) {
      verify(remoteTask, times(1)).start()
      verify(remoteTask, never).kill()
      verify(remoteTask, never).close()
    }
  }

  "CloseRemoteTask" in {
    val commands = List(
      CloseRemoteTask(remoteTasks(0).id, kill = false),
      CloseRemoteTask(remoteTasks(1).id, kill = true))
    for (command ← commands) {
      val response = Await.result(remoteTaskHandler.executeCommand(command), 3.seconds)
      inside(response) { case CloseRemoteTaskResponse ⇒ }
    }
    verify(remoteTasks(0), times(1)).start()
    verify(remoteTasks(0), never).kill()
    verify(remoteTasks(0), times(1)).close()

    verify(remoteTasks(1), times(1)).start()
    verify(remoteTasks(1), times(1)).kill()
    verify(remoteTasks(1), times(1)).close()
  }
}

private object RemoteTaskProcessorTest {
  private val RemoteTaskIds = List(111111111111111111L, 222222222222222222L) map RemoteTaskId.apply
  private val JavaOptions = "JAVA-OPTIONS"
  private val JavaClasspath = "JAVA-CLASSPATH"

  private class TestModule(remoteTasks: List[RemoteTask]) extends AbstractModule {
    private val remoteTaskIterator = remoteTasks.iterator

    def configure() = {}

    @Provides @Singleton
    private def newRemoteTask: StartConfiguration ⇒ RemoteTask = { conf: StartConfiguration ⇒
      conf.javaOptions shouldEqual JavaOptions
      conf.javaClasspath shouldEqual JavaClasspath
      val remoteTask = remoteTaskIterator.synchronized { remoteTaskIterator.next() }
      verifyNoMoreInteractions(remoteTask)
      when(remoteTask.id) thenReturn conf.remoteTaskId
      remoteTask
    }

    @Provides @Singleton
    private def newRemoteTaskId: () ⇒ RemoteTaskId = RemoteTaskIds.synchronized { RemoteTaskIds.iterator.next }
  }

  def waiterOnComplete[A](w: Waiter, future: Future[A])(pf: PartialFunction[Try[A], Unit]): Unit =
    future.onComplete(pf orElse {
      case Failure(t) ⇒ w { fail(t.toString, t) }
      case o ⇒ w { fail(s"Unexpected: $o") }
    })
}
