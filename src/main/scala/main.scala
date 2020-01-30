import java.io.InputStream


import scala.io.{Codec, Source}
import java.io.File


object Main {
  def main(args: Array[String]) {

    val iFile="inverseIndex.bin"
    val c=new Controller()
    c.createInverseIndexJSON("data" + File.separator + "tabelle2017.json")
    c.saveInverseIndex(iFile)



  }

}

