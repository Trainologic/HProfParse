import scodec.bits.BitVector
import java.io.FileInputStream
import scodec._
import scodec.bits._
import codecs._
import scalaz.CharSet
import java.nio.charset.Charset
object t1 {
val bb = hex"0xe4a080"                            //> bb  : scodec.bits.ByteVector = ByteVector(3 bytes, 0xe4a080)

string(Charset.forName("UTF-8")).decodeValue(BitVector(bb))
                                                  //> res0: scodec.Attempt[String] = Successful(ä €)


}