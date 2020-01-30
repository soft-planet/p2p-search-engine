import java.io.InputStream


import scala.io.{Codec, Source}
import java.io.File


object Main {
  // argument ist 201X, X = 6,7,8 oder 9
  def main(args: Array[String]) {

    val arglist = args.toList
    val iFile="inverseIndex.bin"
    val c=new Controller()
    c.createInverseIndexJSON( "src/main/resources/tabelle" +arglist(0) + ".json")//"data" + File.separator + "tabelle2017.json")
    c.saveInverseIndex(iFile)



  }

}

