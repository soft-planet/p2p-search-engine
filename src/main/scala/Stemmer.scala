class Stemmer(var tokens: Set[String]) {
 def stemm()=tokens.map(GermanStemmer.apply(_))

}
