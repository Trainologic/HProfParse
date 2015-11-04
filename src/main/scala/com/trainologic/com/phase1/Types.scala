package com.trainologic.com.phase1
import scodec._
import scodec.bits._
import codecs._
import Utils._
object Types {
  abstract sealed class BasicType(val code: Byte, val size: Int) {
    def decodeValueCodec :Codec[Value] 
  }
  case class ArrayType(idSize: Int) extends BasicType(1, idSize) {
    override val decodeValueCodec = fromIdSize(idSize).as[ArrayObjValue]
  }
  case class ObjectType(idSize: Int) extends BasicType(2, idSize) {
	  override val decodeValueCodec = fromIdSize(idSize).as[ObjValue]
  }
  case object BooleanType extends BasicType(4, 1) {
	  override val decodeValueCodec = bool(8).as[BooleanValue]
    
  }
  case object CharType extends BasicType(5, 2) {
	  override val decodeValueCodec = short16.asDecoder.map(_.toChar).decodeOnly.as[CharValue] 
    
  }
  case object FloatType extends BasicType(6, 4) {
	  override val decodeValueCodec = float.as[FloatValue] 
    
  }
  case object DoubleType extends BasicType(7, 8) {
	  override val decodeValueCodec = double.as[DoubleValue] 
    
  }
  case object ByteType extends BasicType(8, 1){
	  override val decodeValueCodec = byte.as[ByteValue] 
    
  }
  case object ShortType extends BasicType(9, 2){
	  override val decodeValueCodec = short16.as[ShortValue] 
    
  }
  case object IntType extends BasicType(10, 4){
	  override val decodeValueCodec = int32.as[IntValue] 
    
  }
  case object LongType extends BasicType(11, 8){
	  override val decodeValueCodec = int64L.as[LongValue] 
    
  }

  abstract sealed class Value
  case class ArrayObjValue(id: Long) extends Value
  case class ObjValue(id: Long) extends Value
  case class BooleanValue(value : Boolean) extends Value  
  case class CharValue(value: Char) extends Value  
  case class FloatValue(value: Float) extends Value  
  case class DoubleValue(value: Double) extends Value  
  case class ByteValue(value: Byte) extends Value  
  case class ShortValue(value: Short) extends Value  
  case class IntValue(value: Int) extends Value  
  case class LongValue(value: Long) extends Value  
  
}

