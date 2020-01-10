import java.io.InputStream


import scala.io.{Codec, Source}



object Main {
  def main(args: Array[String]) {


    val ccontroller=new Controller()
    val iv=ccontroller.createInverseIndexCSV("tabelle2019.csv")

   println(ccontroller.search("Räuberin Adlershof"))


  }

}

