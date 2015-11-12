import scodec.bits.BitVector
import java.io.FileInputStream
import scodec._
import scodec.bits._
import codecs._


import com.trainologic.com.phase1._

object tt1 extends App {

  val path = """f:\todelete\gf.hprof"""

  
  val fc = new FileInputStream(path).getChannel
  try {

    val xxx = discriminated[Any].by(byte).typecase(1, UTF8.utf8codec(8)).
      typecase(2, LOADCLASS.loadclasscodec(8)).
      typecase(5, TRACE.tracecodec(8)).
      typecase(4, FRAME.framecodec(8)).
      typecase(12, HEAPDUMP.heapdumpcodec(8))
    //     typecase(6, Tag.tagCodec ~ codecs.bits(16L) ~ float ~ uint32 ~ uint32 ~ int64 ~ int64 ~ uint32)

    //val xx = (Header.headerCodec.flatZip(h => UTF8.utf8codec(h.sizeOfIdentifiers.toInt))~ byte).decodeValue(bitVector)
    //val xx = (Header.headerCodec ~ listOfN(provide(154319), xxx)).decodeValue(bitVector)

      
      
      val bitVector = BitVector.fromMmap(fc, 1024 * 1000 * 150)
      println(s"size: ${bitVector.size}")
//    val xx = (Header.headerCodec ~ listOfN(provide(170298), xxx)).decodeValue(bitVector)
    val xx = (Header.headerCodec ~ list(xxx)).decodeValue(bitVector)

    //xx.fold(println, x => println(x._2.last.asInstanceOf[Tuple2[_,_]]._2.asInstanceOf[ByteVector].drop(1+7*8+4).take(100)))
  //  xx.fold(println, x => println(x._2.last))

  } finally {
    fc.close()
  }

}