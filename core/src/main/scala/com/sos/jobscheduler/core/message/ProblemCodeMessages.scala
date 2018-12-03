package com.sos.jobscheduler.core.message

import com.sos.jobscheduler.base.problem.ProblemCode
import com.sos.jobscheduler.common.configutils.Configs
import com.sos.jobscheduler.common.utils.JavaResource
import scala.util.Try

/**
  * @author Joacim Zschimmer
  */
object ProblemCodeMessages
{
  private val resource = JavaResource("com/sos/jobscheduler/core/message/messages-en.conf")
  private val config = Configs.loadResource(resource)

  implicit val problemCodeToString: ProblemCode ⇒ Option[String] =
    code ⇒ Try(config.getString(code.string)).toOption
}
