import java.io.InputStream


import scala.io.{Codec, Source}
import java.io.File


object Main {
  // argument ist src/main/resources/tabelle201X.json, X = 6,7,8 oder 9
  def main(args: Array[String]) {

    val arglist = args.toList
    val iFile="inverseIndex.bin"
    val c=new Controller()
    c.createInverseIndexJSON( arglist(0))//"data" + File.separator + "tabelle2017.json")
    c.saveInverseIndex(iFile)



  }

}

