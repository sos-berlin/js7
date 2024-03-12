package js7.tests.feed

import cats.effect.Resource
import io.circe.syntax.EncoderOps
import js7.base.circeutils.CirceUtils.RichJson
import js7.base.configutils.Configs.HoconStringInterpolator
import js7.base.data.ByteArray
import js7.base.test.OurTestSuite
import js7.base.thread.CatsBlocking.syntax.*
import js7.base.time.ScalaTime.*
import js7.base.utils.ScalaUtils.syntax.RichEither
import js7.data.item.ItemOperation.AddOrChangeSimple
import js7.data.lock.{Lock, LockPath}
import js7.tests.testenv.ControllerAgentForScalaTest
import cats.effect.IO

final class FeedMainTest extends OurTestSuite, ControllerAgentForScalaTest:

  override protected def controllerConfig = config"""
    js7.auth.users.TEST-USER.permissions = [ UpdateItem ]
    """
  protected def agentPaths = Nil
  protected def items = Nil

  "test" in:
    val ops = Vector[Any](AddOrChangeSimple(Lock(LockPath("TEST"))))
    implicit val opJsonCodec = Feed.opJsonCodec
    val in = Resource.eval(IO.pure(ByteArray(ops.asJson.compactPrint).toInputStream))
    FeedMain.run(Seq(s"--controller=${controller.localUri}", "--user=TEST-USER:TEST-PASSWORD"), in)
      .await(99.s).orThrow
