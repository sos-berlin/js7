package js7.data.jobapi

/**
 * These method signatures for sos.spooler.Job_impl are used generally in C++.
 *
 * @author Joacim Zschimmer
 */
object JavaJobSignatures:
//val SpoolerInitSignature = "spooler_open()Z"  // Z: returns Boolean
  val SpoolerExitSignature = "spooler_exit()V"  // V: returns Unit
  val SpoolerOpenSignature = "spooler_open()Z"
//val SpoolerCloseSignature = "spooler_open()V"
//val SpoolerProcessSignature = "spooler_open()Z"
  val SpoolerOnSuccessSignature = "spooler_on_success()V"
  val SpoolerOnErrorSignature = "spooler_on_error()V"
