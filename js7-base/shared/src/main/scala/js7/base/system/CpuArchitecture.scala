package js7.base.system

final case class CpuArchitecture(officialName: String, visualName: String)

object CpuArchitecture
{
  val x86 = CpuArchitecture("x86", "Win32")
  val x64 = CpuArchitecture("x64", "x64")

  val cpuArchitecture: CpuArchitecture =
    if (System.getProperty("os.arch") == "amd64")
      x64
    else
      x86
}
