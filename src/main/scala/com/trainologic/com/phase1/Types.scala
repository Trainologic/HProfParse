package com.trainologic.com.phase1
import scodec._
import scodec.bits._
import codecs._
import Utils._
object Types {
  
  object BasicType {
    val decoder: Codec[BasicType] = 
      discriminated[BasicType].by(byte).typecase(1,  provide(ArrayType))
      .typecase(2,  provide(ObjectType))
      .typecase(4,  provide(BooleanType))
      .typecase(5,  provide(CharType))
      .typecase(6,  provide(FloatType))
      .typecase(7,  provide(DoubleType))
      .typecase(8,  provide(ByteType))
      .typecase(9,  provide(ShortType))
      .typecase(10,  provide(IntType))
      .typecase(11,  provide(LongType))
  }
  
  abstract sealed class BasicType {
    def codec(idCodec: Codec[Long]): Decoder[Value]
  }
  
  case object ArrayType extends BasicType {
	  override def codec(idCodec: Codec[Long]) = idCodec.as[ArrayObjValue]
  }
  
  
  case object ObjectType extends BasicType {
	   override def codec(idCodec: Codec[Long]) = idCodec.as[ObjValue]
  }
  case object BooleanType extends BasicType{
	   override def codec(idCodec: Codec[Long]) = bool(8).as[BooleanValue]
    
  }  
  case object CharType extends BasicType {
	   override def codec(idCodec: Codec[Long]) = short16.decoderOnlyMap(_.toChar).as[CharValue] 
  }
  case object FloatType extends BasicType {
	   override def codec(idCodec: Codec[Long]) = float.as[FloatValue] 
    
  }
  case object DoubleType extends BasicType {
	   override def codec(idCodec: Codec[Long]) = double.as[DoubleValue] 
    
  }
  case object ByteType extends BasicType{
	   override def codec(idCodec: Codec[Long]) = byte.as[ByteValue] 
    
  }
  case object ShortType extends BasicType{
	   override def codec(idCodec: Codec[Long]) = short16.as[ShortValue] 
    
  }
  case object IntType extends BasicType{
	   override def codec(idCodec: Codec[Long]) = int32.as[IntValue] 
    
  }
  case object LongType extends BasicType{
	   override def codec(idCodec: Codec[Long]) = int64L.as[LongValue] 
    
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

