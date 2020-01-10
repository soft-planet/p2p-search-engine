import java.io.InputStream

import Main.getClass

import scala.io.{Codec, Source}



object Test {
  def main(args: Array[String]) {

  val test="Chanel ist der beste Programmierer. chanel Was hast du heute abend vor? Heute mache ich Sport. Übelste höhen Ängste"
  val inverseIndex= new InverseIndex
  inverseIndex.insert("doc1",test)
  inverseIndex.insert("doc2","chanel")

    println(inverseIndex.inverseIndex)
  inverseIndex.remove("doc1","chanel")
    println(inverseIndex.inverseIndex)

  }

}

