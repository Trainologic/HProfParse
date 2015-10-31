package com.trainologic.com.phase1

import scodec.bits.BitVector
import java.io.FileInputStream
import scodec._
import scodec.bits._
import codecs._

sealed abstract class Tag(timestamp: Long)

object Tag {
  val microsecs = uint32
  val length = uint32
  val tagCodec = (microsecs ~ length)

  def tagDec[A](c: Codec[A]) = Tag.tagCodec.flatZip(x => paddedFixedSizeBytes(x._2, c, ignore(1)))
  def fromIdSize(idSize: Int) = idSize match {
    case 8 => int64
    case 4 => uint32
  }

}

case class UTF8(timestamp: Long, id: Long, str: String) extends Tag(timestamp)
object UTF8 {
  def utf8codec(idSize: Int) = {
    val idCodec = Tag.fromIdSize(idSize)
    //      Tag.tagCodec.flatZip(x => (idCodec ~ limitedSizeBytes(x._2 - idSize, fallback(codecs.bits.asDecoder.map(_ => "*unknown*").decodeOnly, utf8).asDecoder.map(_.fold(identity, identity)).decodeOnly))).
    //        map(z => UTF8(z._1._1, z._2._1, z._2._2)).decodeOnly

    Tag.tagDec(int64 ~ fallback(codecs.bits.asDecoder.map(_ => "*unknown*").decodeOnly, utf8).asDecoder.map(_.fold(identity, identity)).decodeOnly).
      map(z => UTF8(z._1._1, z._2._1, z._2._2)).decodeOnly

  }
}

//def mutf8Codec = Tag.tagCodec.flatZip(x => paddedFixedSizeBytes(x._2, int64 ~ utf8, ignore(1)))

case class LOADCLASS(timestamp: Long, serialNum: Long, classId: Long, stacktraceNum: Long, name: Long) extends Tag(timestamp)
object LOADCLASS {
  def loadclasscodec(idSize: Int) = {
    val idCodec = Tag.fromIdSize(idSize)

    Tag.tagDec(uint32 ~ idCodec ~ uint32 ~ idCodec).asDecoder.map {
      case (p1, (((p2, p3), p4), p5)) => ((((p1._1, p2), p3), p4), p5)
    }.decodeOnly.flattenLeftPairs.as[LOADCLASS]

    //    ((uint32 <~ ignore(32)) ~ uint32 ~ idCodec ~ uint32 ~ idCodec).flattenLeftPairs.as[LOADCLASS]

  }
}

case class TRACE(timestamp: Long, serialNum: Long, threadNum: Long, stackFrameIds: List[Long]) extends Tag(timestamp)
object TRACE {
  def tracecodec(idSize: Int) = {
    val idCodec = Tag.fromIdSize(idSize)

    Tag.tagDec(uint32 ~ uint32 ~ uint32 ~ list(idCodec)).asDecoder.map {
      case (p1, (((p2, p3), p4), p5)) => (((p1._1, p2), p3), p5)
    }.decodeOnly.flattenLeftPairs.as[TRACE]

    /*   Tag.tagCodec.flatZip(x => (uint32 ~ uint32 ~ uint32 ~ listOfN(provide(x._2.toInt - 32 - 32 - 32), idCodec))).asDecoder.
      map(x => { println(x._1._2); TRACE(x._1._1, x._2._1._1._1, x._2._1._1._2, x._2._2) }).decodeOnly

 */ }
}
case class FRAME(timestamp: Long, stackFrameId: Long, methodNameId: Long, methodSignatureId: Long, sourceFileNameId: Long, classSerialNum: Long, lineNumber: Int) extends Tag(timestamp)
object FRAME {
  def framecodec(idSize: Int) = {
    val idCodec = Tag.fromIdSize(idSize)
    Tag.tagDec(idCodec ~ idCodec ~ idCodec ~ idCodec ~ uint32 ~ int32).asDecoder.map {
      case (p1, (((((p2, p3), p4), p5), p6), p7)) => ((((((p1._1, p2), p3), p4), p5), p6), p7)
    }.decodeOnly.flattenLeftPairs.as[FRAME]

    // ((uint32 <~ ignore(32)) ~ idCodec ~ idCodec ~ idCodec ~ idCodec ~ uint32 ~ int32).flattenLeftPairs.as[FRAME]

  }
}
case class Header(version: String, sizeOfIdentifiers: Long, timestamp: Long)
object Header {
  val headerCodec = (cstring :: uint32 :: int64).as[Header]
}
