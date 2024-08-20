package js7.launcher.crashpidfile

import cats.effect.IO
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.io.process.Pid
import js7.base.test.OurAsyncTestSuite

final class CrashPidFileTest extends OurAsyncTestSuite:

  "test" in:
    temporaryFileResource[IO]()
      .flatMap: file =>
        CrashPidFileService.file(file).map(file -> _)
      .use: (file, pidFile) =>
        def readHex = file.byteArray.iterator.map(o => f"$o%02x").mkString
        def add(pid: Pid) = pidFile.register(pid).allocated.map(_._2)
        for
          rel1 <- add(Pid(1))
          _ = assert(readHex ==
            "0000000000000001")

          _ <- rel1
          _ = assert(readHex == "")

          rel2 <- add(Pid(2))
          rel3 <- add(Pid(3))
          rel123 <- add(Pid(0x1234567890123456L))
          _ = assert(readHex ==
            "0000000000000002" +
            "0000000000000003" +
            "1234567890123456")

          _ <- rel3
          _ = assert(readHex ==
            "0000000000000002" +
            "0000000000000000" +
            "1234567890123456")

          _ <- rel123
          _ = assert(readHex ==
            "0000000000000002")

          _ <- rel2
          _ = assert(readHex == "")

          _ <- add(Pid(Long.MinValue))
          _ <- add(Pid(Long.MaxValue))
          _ = assert(readHex ==
            "8000000000000000" +
            "7fffffffffffffff")
        yield
          succeed
