package com.trainologic.com.phase1
import scodec._
import scodec.codecs._
object Utils {
  
  implicit class CodecOps[A](val ca: Codec[A]) extends AnyVal {
    def decoderOnlyMap[B](f: A=>B): Codec[B] = ca.asDecoder.map(f).decodeOnly
  }
  
  
  
  def fromIdSize(idSize: Int) = idSize match {
    case 8 => int64
    case 4 => uint32
  }
}