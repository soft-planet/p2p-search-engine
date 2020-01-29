import java.io.{FileInputStream, ObjectInputStream}

import scala.io.Codec

class PeerDiscovery {

  def getPeers() ={

    scala.io.Source.fromInputStream( new FileInputStream("discovery-config.txt") )(Codec("ISO-8859-1")).getLines().toSet
  }

}
