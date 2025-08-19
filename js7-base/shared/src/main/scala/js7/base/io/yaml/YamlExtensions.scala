package js7.base.io.yaml

import cats.effect.SyncIO
import io.circe.{Decoder, Json}
import io.circe.yaml.scalayaml
import js7.base.catsutils.CatsEffectExtensions.run
import js7.base.circeutils.CirceUtils.*
import js7.base.data.ByteSequence
import js7.base.data.ByteSequence.ops.*
import js7.base.io.JavaResource
import js7.base.problem.Checked

object YamlExtensions:

  extension (string: String)
    def parseYamlAs[A: Decoder]: Checked[A] =
      yamlToJson.flatMap(_.as[A].toChecked)

    def yamlToJson: Checked[Json] =
      scalayaml.parser.parse(string).toChecked


  extension [ByteSeq](byteSeq: ByteSeq)
    def yamlAs[A: Decoder](using ByteSequence[ByteSeq]): Checked[A] =
      byteSeq.utf8String.parseYamlAs[A]


  extension (javaResource: JavaResource)
    def yamlAs[A: Decoder]: Checked[A] =
      javaResource.reader[SyncIO].use: reader =>
        SyncIO:
          scalayaml.parser.parse(reader)
      .run()
      .flatMap(_.as[A])
      .toChecked
