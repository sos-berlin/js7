package js7.tests.feed

import cats.effect.Resource
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.data.ByteArray
import js7.base.thread.MonixBlocking.syntax.RichTask
import js7.base.time.ScalaTime._
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.lock.{Lock, LockPath}
import js7.tests.testenv.ControllerAgentForScalaTest
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest.freespec.AnyFreeSpec

final class FeedMainTest extends AnyFreeSpec with ControllerAgentForScalaTest
{
  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """
  protected def agentIds = Nil
  protected def versionedItems = Nil

  "test" in {
    val ops = Vector[Any](AddOrChangeSimple(Lock(LockPath("TEST"))))
    implicit val opJsonCodec = Feed.opJsonCodec
    val in = Resource.eval(Task.pure(ByteArray(ops.asJson.compactPrint).toInputStream))
    FeedMain.run(Array(s"--controller=${controller.localUri}", "--user=TEST-USER:TEST-PASSWORD"), in)
      .await(99.s).orThrow
  }
}
