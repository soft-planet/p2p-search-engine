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

    def getSimilarity(doc1 : CosineSimilarityDocument[T], doc2: CosineSimilarityDocument[T]): Double = {
        dotProduct(getTfidf(doc1), getTfidf(doc2))
    }
    private val defaultIdf : Double = math.log(documentCount)
    def getTfidf(doc : CosineSimilarityDocument[T]): Map[T, Double] = 
        doc.getTf.map(x => (x._1, x._2 * idfs.getOrElse(x._1, defaultIdf))).toMap
}

trait CosineSimilarityDocument[T]{
    def size: Int
    def getTokenCounts: Map[T, Int]
    def getTf: Map[T, Double] = getTokenCounts.view.mapValues(_.toDouble / size).toMap
}

object CosineSimilarity{

    implicit class IterableCosineSimitarityDocument[T](it: Iterable[T]) extends CosineSimilarityDocument[T]{
        override def size: Int = it.size
        override def getTokenCounts: Map[T,Int] = it.groupBy(identity).view.mapValues(_.size).toMap
    }

    implicit class TokenCountsCosineSimilarityDocument[T](tc: Map[T, Int])extends CosineSimilarityDocument[T]{
        override def size: Int = (List(0) ++ tc.values).sum
        override def getTokenCounts: Map[T,Int] = tc
    }

    def apply[F](index: InvertedIndex[_, F, _]): CosineSimilarity[F] = {
        val docsForFeature = index.index
            .mapValues(docs => docs.map(_._1))
        val documentCount = docsForFeature.values.fold(Set())(_ ++ _).size
        val featureCounts = docsForFeature.mapValues(_.size)
        val idfs = featureCounts.view.mapValues(x => math.log(documentCount / x.toDouble)).toMap
        new CosineSimilarity[F](idfs, documentCount)
    }

    def sparse[T](relevantDocs: Iterable[CosineSimilarityDocument[T]], completeDocCount: Int): CosineSimilarity[T] = {
        val idfs = relevantDocs.flatMap(_.getTokenCounts.keys.toVector)
                    .groupBy(identity)
                    .view
                    .mapValues(_.size)
                    .mapValues(x => math.log(completeDocCount / x.toDouble))
                    .toMap
        new CosineSimilarity[T](idfs, completeDocCount)
    }
}