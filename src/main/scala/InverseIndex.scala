class InverseIndex{

  var inverseIndex = Map("" -> Set(""))



  def insert(uri: String, text: String): Unit = {
   val uriSet = Set(uri)
    val result = transform(text).map(_ -> uriSet)
    result.foreach((token) => {
      val newSet = this.inverseIndex.get(token._1).getOrElse(Set()) ++ token._2
      this.inverseIndex=this.inverseIndex-(token._1)
      this.inverseIndex=this.inverseIndex+(token._1->newSet)
    })

  }

  def remove(uri: String, text: String) = {
    val uriSet = Set(uri)
    val result = transform(text).map(_ -> uriSet)


    result.foreach(token => {

      val oldValue = this.inverseIndex.get(token._1)

      val newValue = oldValue.get diff token._2
      this.inverseIndex = this.inverseIndex - (token._1)
      this.inverseIndex = this.inverseIndex + ((token._1, newValue))
    })
  }


  def transform(text: String) = {
    val tokens = new Tokenizer("german", text).tokenize();
    val stemms = new Stemmer(tokens).stemm()
    val result = new Stopwords("german").remove(stemms)
    result
  }
}
