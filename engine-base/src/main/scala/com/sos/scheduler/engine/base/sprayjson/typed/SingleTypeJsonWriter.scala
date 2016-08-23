package com.sos.scheduler.engine.base.sprayjson.typed

import spray.json.{JsObject, JsString, RootJsonWriter}

/**
  * @author Joacim Zschimmer
  */
private[typed] class SingleTypeJsonWriter[A](typeField: (String, JsString), jsonWriter: RootJsonWriter[A])
extends RootJsonWriter[A] {

  def write(a: A): JsObject =
    jsonWriter.write(a).asJsObject match {
      case jsObject if jsObject.fields contains typeField._1 ⇒
        sys.error(s"Serialized object contains field $typeField")
        jsObject
      case jsObject ⇒
        JsObject(jsObject.fields + typeField)
    }
}
