package com.trainologic.com.phase1

import scodec.bits.BitVector
import java.io.FileInputStream
import scodec._
import scodec.bits._
import codecs._
import com.trainologic.com.phase1.Types._
import Utils._
sealed abstract class Tag(timestamp: Long)

object Tag {
  val microsecs = uint32
  val length = uint32
  val tagCodec = (microsecs ~ length)

  def tagDec[A](c: Codec[A]) = Tag.tagCodec.flatZip(x => paddedFixedSizeBytes(x._2, c, ignore(1)))

  def decode[A, B](c: Codec[A])(f: ((((Long, Long), A)) => B)): Codec[B] =
    Tag.tagDec(c).decoderOnlyMap(f)
}

case class UTF8(timestamp: Long, id: Long, str: String) extends Tag(timestamp)
object UTF8 {
  def utf8codec(idSize: Int) = {
    val idCodec = fromIdSize(idSize)
    Tag.tagDec(int64 ~ fallback(codecs.bits.decoderOnlyMap(_ => "*unknown*"), utf8).decoderOnlyMap(_.fold(identity, identity))).
      map(z => UTF8(z._1._1, z._2._1, z._2._2)).decodeOnly

  }
}
case class LOADCLASS(timestamp: Long, serialNum: Long, classId: Long, stacktraceNum: Long, name: Long) extends Tag(timestamp)
object LOADCLASS {
  def loadclasscodec(idSize: Int) = {
    val idCodec = fromIdSize(idSize)

    Tag.decode(uint32 ~ idCodec ~ uint32 ~ idCodec) {
      case (p1, (((p2, p3), p4), p5)) => ((((p1._1, p2), p3), p4), p5)
    }.flattenLeftPairs.as[LOADCLASS]
  }
}

case class TRACE(timestamp: Long, serialNum: Long, threadNum: Long, stackFrameIds: List[Long]) extends Tag(timestamp)
object TRACE {
  def tracecodec(idSize: Int) = {
    val idCodec = fromIdSize(idSize)

    Tag.decode(uint32 ~ uint32 ~ uint32 ~ list(idCodec)) {
      case (p1, (((p2, p3), p4), p5)) => (((p1._1, p2), p3), p5)
    }.flattenLeftPairs.as[TRACE]
  }
}
case class FRAME(timestamp: Long, stackFrameId: Long, methodNameId: Long, methodSignatureId: Long,
                 sourceFileNameId: Long, classSerialNum: Long, lineNumber: Int) extends Tag(timestamp)
object FRAME {
  def framecodec(idSize: Int) = {
    val idCodec = fromIdSize(idSize)
    Tag.decode(idCodec ~ idCodec ~ idCodec ~ idCodec ~ uint32 ~ int32) {
      case (p1, (((((p2, p3), p4), p5), p6), p7)) => ((((((p1._1, p2), p3), p4), p5), p6), p7)
    }.flattenLeftPairs.as[FRAME]
  }
}

abstract sealed class HeapDumpRecord
object HeapDumpRecord {
  def heapDumpRecordCodec(idCodec: Codec[Long]) = discriminated[HeapDumpRecord].by(uint8).
    typecase(0xFF, idCodec.as[HPROF_GC_ROOT_UNKNOWN]).
    typecase(0x08, (idCodec ~ uint32 ~ uint32).flattenLeftPairs.as[HPROF_GC_ROOT_THREAD_OBJ]).
    typecase(0x01, (idCodec ~ idCodec).flattenLeftPairs.as[HPROF_GC_ROOT_JNI_GLOBAL]).
    typecase(0x02, (idCodec ~ uint32 ~ uint32).flattenLeftPairs.as[HPROF_GC_ROOT_JNI_LOCAL]).
    typecase(0x04, (idCodec ~ uint32).flattenLeftPairs.as[HPROF_GC_ROOT_NATIVE_STACK]).
    typecase(0x06, (idCodec ~ uint32).flattenLeftPairs.as[HPROF_GC_ROOT_THREAD_BLOCK]).
    typecase(0x05, (idCodec).as[HPROF_GC_ROOT_STICKY_CLASS]).
    typecase(0x07, (idCodec).as[HPROF_GC_ROOT_MONITOR_USED]).
    typecase(0x03, (idCodec ~ uint32 ~ uint32).flattenLeftPairs.as[HPROF_GC_ROOT_JAVA_FRAME]).
    typecase(0x20, ((idCodec ~ uint32 ~ uint32 ~ uint32 ~ uint32 ~ uint32 <~ ignore(64)) ~ uint32 ~ 
        ConstantPool.codec ~ HPROF_GC_CLASS_DUMP.staticFieldsCodec(idCodec) ~ 
        HPROF_GC_CLASS_DUMP.instanceFieldsCodec(idCodec)).flattenLeftPairs.as[HPROF_GC_CLASS_DUMP])
}
case class HPROF_GC_ROOT_UNKNOWN(objId: Long) extends HeapDumpRecord
case class HPROF_GC_ROOT_THREAD_OBJ(threadObjId: Long, seqNum: Long, stackTraceSeqNum: Long) extends HeapDumpRecord
case class HPROF_GC_ROOT_JNI_GLOBAL(objId: Long, jniGlobalRefId: Long) extends HeapDumpRecord
case class HPROF_GC_ROOT_JNI_LOCAL(objId: Long, threadSerNum: Long, frameNum: Long) extends HeapDumpRecord
case class HPROF_GC_ROOT_JAVA_FRAME(objId: Long, threadSerNum: Long, frameNum: Long) extends HeapDumpRecord
case class HPROF_GC_ROOT_NATIVE_STACK(objId: Long, threadSerNum: Long) extends HeapDumpRecord
case class HPROF_GC_ROOT_THREAD_BLOCK(objId: Long, threadSerNum: Long) extends HeapDumpRecord

case class StaticField(fieldName: Long, fieldType: BasicType, value: Value)
object StaticField {
	def codec(idCodec: Codec[Long]) : Codec[StaticField] = (idCodec ~ BasicType.decoder ~ Value.decoder).flattenLeftPairs.as[StaticField]
  
}

case class InstanceField(fieldName: Long, fieldType: BasicType)
object InstanceField {
	def codec(idCodec: Codec[Long]) : Codec[InstanceField] = (idCodec ~ BasicType.decoder).flattenLeftPairs.as[InstanceField]
  
}
case class ConstantPoolEntry(index: Int, entryType: BasicType, value: Value)
object ConstantPoolEntry {
  def codec : Codec[ConstantPoolEntry] = (uint16 ~ BasicType.decoder ~ Value.decoder).flattenLeftPairs.as[ConstantPoolEntry]
}
case class ConstantPool(entries: List[ConstantPoolEntry])
object ConstantPool {
  def codec: Codec[ConstantPool] = uint16.flatMap(paddedFixedSizeBytes(_, list(ConstantPoolEntry.codec), ignore(1))).decodeOnly.as[ConstantPool] 
}

case class HPROF_GC_CLASS_DUMP(clzObjId: Long, stackTraceSeqNum: Long, superClzObjId: Long,
                               clzLoaderObjId: Long, signersObjId: Long, protectionDomainObjId: Long,
                               instanceSize: Long, constantPool: ConstantPool, staticFields: List[StaticField], instanceFields: List[InstanceField]) extends HeapDumpRecord
object HPROF_GC_CLASS_DUMP {
  def staticFieldsCodec(idCodec: Codec[Long]): Codec[List[StaticField]] = 
    uint16.flatMap(paddedFixedSizeBytes(_, list(StaticField.codec(idCodec)), ignore(1))).decodeOnly
  def instanceFieldsCodec(idCodec: Codec[Long]): Codec[List[InstanceField]] = 
    uint16.flatMap(paddedFixedSizeBytes(_, list(InstanceField.codec(idCodec)), ignore(1))).decodeOnly
}

case class HPROF_GC_ROOT_STICKY_CLASS(objId: Long) extends HeapDumpRecord
case class HPROF_GC_ROOT_MONITOR_USED(objId: Long) extends HeapDumpRecord

case class HEAPDUMP(timestamp: Long, heapDumpRecords: List[HeapDumpRecord])
object HEAPDUMP {
  def heapdumpcodec(idSize: Int) = {
    val idCodec = fromIdSize(idSize)

  }
}

case class Header(version: String, sizeOfIdentifiers: Long, timestamp: Long)
object Header {
  val headerCodec = (cstring :: uint32 :: int64).as[Header]
}
