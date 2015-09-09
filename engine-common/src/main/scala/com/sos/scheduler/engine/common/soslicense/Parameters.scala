package com.sos.scheduler.engine.common.soslicense

import com.sos.scheduler.engine.common.soslicense.LicenseKey.Parameter

/**
 * @author Joacim Zschimmer
 */
object Parameters {
  val JobScheduler = Parameter("7")
  val ValidIn1900 = Parameter("19")
  val ValidIn2000 = Parameter("20")
  val OperatingSystems = Parameter("1A")
  val ClassicAgent = Parameter("22")
  val UniversalAgent = Parameter("23")
  val ZZ = Parameter("ZZ")

  val ZZIncludes = Set(JobScheduler, ClassicAgent, UniversalAgent)

  val parameterToString = Map[Parameter, String](
    JobScheduler → "JobScheduler",
    ClassicAgent → "Classic Agent",
    UniversalAgent → "Universal Agent")
  .withDefault { _.string }
}
