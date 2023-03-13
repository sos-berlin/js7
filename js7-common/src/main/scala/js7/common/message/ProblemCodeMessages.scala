package js7.common.message

import js7.base.configutils.Configs
import js7.base.io.JavaResource
import js7.base.problem.{CodedMessages, ProblemCode}
import monix.execution.atomic.AtomicBoolean
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
object ProblemCodeMessages
{
  private val resource = JavaResource("js7/common/message/messages-en.conf")
  private val config = Configs.loadResource(resource)

  private[message] val problemCodeToPattern: ProblemCode => Option[String] =
    code => Try(config.getString(code.string)).toOption
  private val once = AtomicBoolean(false)

  def initialize(): Unit =
    if (!once.get()) synchronized {
      if (!once.getAndSet(true)) {
        CodedMessages.codeToPattern = problemCodeToPattern
      }
    }
}
