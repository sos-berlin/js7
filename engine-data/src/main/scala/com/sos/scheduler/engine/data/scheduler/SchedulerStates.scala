package com.sos.scheduler.engine.data.scheduler

import spray.json.{JsString, JsValue, JsonFormat}

/**
  * @author Joacim Zschimmer
  */
object SchedulerStates {
  implicit val SchedulerStateJsonFormat: JsonFormat[SchedulerState] =
    new JsonFormat[SchedulerState] {
      def write(o: SchedulerState) = JsString(o.cppName)
      def read(v: JsValue) = SchedulerState.ofCppName(v.asInstanceOf[JsString].value)
    }
}
