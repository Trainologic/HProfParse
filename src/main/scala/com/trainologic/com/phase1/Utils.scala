package com.trainologic.com.phase1
import scodec.codecs._
object Utils {
  def fromIdSize(idSize: Int) = idSize match {
    case 8 => int64
    case 4 => uint32
  }
}