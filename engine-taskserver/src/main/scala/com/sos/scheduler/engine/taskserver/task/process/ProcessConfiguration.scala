package com.sos.scheduler.engine.taskserver.task.process

import com.sos.scheduler.engine.taskserver.task.process.StdoutStderr.StdoutStderrType
import java.nio.file.Path
import scala.collection.immutable

/**
 * @author Joacim Zschimmer
 */
final case class ProcessConfiguration(
  stdFileMap: Map[StdoutStderrType, Path] = Map(),
  additionalEnvironment: immutable.Iterable[(String, String)] = Map())
