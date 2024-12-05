package js7.launcher.crashpidfile

import cats.effect.IO
import java.nio.charset.StandardCharsets
import java.nio.charset.StandardCharsets.UTF_8
import js7.base.io.file.FileUtils
import js7.base.io.file.FileUtils.syntax.*
import js7.base.io.file.FileUtils.temporaryFileResource
import js7.base.test.OurAsyncTestSuite

final class IndexedRecordSetTest extends OurAsyncTestSuite:

  "textFile" in:
    temporaryFileResource[IO]()
      .flatMap: file =>
        IndexedRecordSet.textFile(file, Long.MinValue.toString.length): (buf, value) =>
          buf.put(value.toString.getBytes(UTF_8))
        .map(file -> _)
      .use: (file, set) =>
        def add(i: Long) = set.register(i).allocated.map(_._2)
        for
          rel1 <- add(1)
          _ = assert(file.contentString ==
            "1                   \n")

          _ <- rel1
          _ = assert(file.contentString == "")

          rel2 <- add(2)
          rel3 <- add(3)
          rel4 <- add(4)
          _ = assert(file.contentString ==
            "2                   \n" +
            "3                   \n" +
            "4                   \n")

          _ <- rel3
          _ = assert(file.contentString ==
            "2                   \n" +
            "                    \n" +
            "4                   \n")

          _ <- rel4
          _ = assert(file.contentString ==
            "2                   \n")

          _ <- rel2
          _ = assert(file.contentString == "")

          _ <- add(0)
          _ <- add(Long.MinValue)
          _ <- add(Long.MaxValue)
          _ = assert(file.contentString ==
            "0                   \n" +
            "-9223372036854775808\n" +
            "9223372036854775807 \n")
        yield
          succeed
