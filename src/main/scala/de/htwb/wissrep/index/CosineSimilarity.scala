package de.htwb.wissrep.index

import scala.math

class CosineSimilarity[T] private (private val idfs : Map[T, Double], private val documentCount: Int){

    private def getLength[X](x : Map[X, Double]): Map[X, Double] =
        x.view.mapValues(l => math.sqrt(l * l)).toMap

    private def dotProduct[X](a: Map[X, Double], b : Map[X, Double]): Double = {
        if(a.size > b.size)
            dotProduct(b, a)
        else
            a.map(x => b.getOrElse(x._1, 0d) * x._2).sum
    }

    def getSimilarity(doc1 : Iterable[T], doc2: Iterable[T]): Double = {
        dotProduct(getTfidf(doc1), getTfidf(doc2))
    }

    def getTf: Iterable[T] => Map[T, Double] = CosineSimilarity.getTf

    private val defaultIdf : Double = math.log(documentCount)
    def getTfidf(doc : Iterable[T]): Map[T, Double] = 
        getTf(doc).map(x => (x._1, x._2 * idfs.getOrElse(x._1, defaultIdf))).toMap
}

object CosineSimilarity{

    def apply[F](index: InvertedIndex[_, F, _]): CosineSimilarity[F] = {
        val docsForFeature = index.index
            .mapValues(docs => docs.map(_._1))
        val documentCount = docsForFeature.values.fold(Set())(_ ++ _).size
        val featureCounts = docsForFeature.mapValues(_.size)
        val idfs = featureCounts.view.mapValues(x => math.log(documentCount / x.toDouble)).toMap
        new CosineSimilarity[F](idfs, documentCount)
    }

    def sparse[T](relevantDocs: Iterable[Iterable[T]], completeDocCount: Int): CosineSimilarity[T] = {
        val idfs = relevantDocs.flatMap(_.iterator.distinct.toVector)
                    .groupBy(identity)
                    .view
                    .mapValues(_.size)
                    .mapValues(x => math.log(completeDocCount / x.toDouble))
                    .toMap
        new CosineSimilarity[T](idfs, completeDocCount)
    }

        def sparse[T](documentApperanceCount: Map[T, Int], completeDocCount: Int): CosineSimilarity[T] = {
        val idfs = documentApperanceCount
                    .view
                    .mapValues(x => math.log(completeDocCount / x.toDouble))
                    .toMap
        new CosineSimilarity[T](idfs, completeDocCount)
    }

    def getTf[T](doc : Iterable[T]): Map[T, Double] ={ 
        val tokenCounts = doc.groupBy(identity).mapValues(_.size)
        val totalTokenCount = tokenCounts.values.sum
        tokenCounts.view.mapValues(_.toDouble / totalTokenCount).toMap
    }
}