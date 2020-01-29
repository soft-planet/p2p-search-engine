import java.io.InputStream


import scala.io.{Codec, Source}



object Main {
  def main(args: Array[String]) {

    val iFile="inverseIndex.bin"
    val c=new Controller()
    c.createInverseIndexJSON("tabelle2017.json")
    c.saveInverseIndex(iFile)



  }

}

