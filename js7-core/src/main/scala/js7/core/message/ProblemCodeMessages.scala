package js7.core.message

import js7.base.problem.{CodedMessages, ProblemCode}
import js7.common.configutils.Configs
import js7.common.utils.JavaResource
import monix.execution.atomic.AtomicBoolean
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
object ProblemCodeMessages
{
  private val resource = JavaResource("js7/core/message/messages-en.conf")
  private val config = Configs.loadResource(resource, internal = true)

  private[message] val problemCodeToPattern: ProblemCode => Option[String] =
    code => Try(config.getString(code.string)).toOption
  private val once = AtomicBoolean(false)

  def initialize(): Unit =
    synchronized {
      if (!once.getAndSet(true)) {
        CodedMessages.codeToPattern = problemCodeToPattern
      }
    }
}
