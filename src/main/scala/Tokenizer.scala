class Tokenizer(var language: String, var text: String) {

  def tokenize() = {

    language match {

      case _ =>simpleTokenize()
    }
  }

  def simpleTokenize() = {
    text.split("[^a-zA-Z0-9äÄüÜöÖß@]").filterNot(_=="").map(x=>x.toLowerCase()).toSet
  }
}
