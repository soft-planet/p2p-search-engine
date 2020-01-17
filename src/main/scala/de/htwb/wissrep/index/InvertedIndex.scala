package de.htwb.wissrep.index

import scala.collection.immutable.Set
import scala.collection.immutable.Map

object InvertedIndex {
    def apply[D, F, I](doc: D, features: Iterable[(F, I)]): InvertedIndex[D, F, I] = {
        val groupedFeatures = features.groupMap(_._1)(data => (doc, data._2))
        val unifiedFeatures = groupedFeatures.view.mapValues(_.toSet).toMap
        new InvertedIndex(unifiedFeatures)
    }

    def apply[D, F, I](docFeatures: (D, Iterable[(F, I)])*): InvertedIndex[D, F, I] =
        combine(docFeatures.map(x => apply(x._1, x._2)))

    def apply[D, F, I](index : Map[F, Set[(D, I)]]): InvertedIndex[D, F, I] = 
        new InvertedIndex(if(index != null) index else Map())
    
    def apply[D, F, I](
        docs: Iterable[D],
        featureExtractor: D => Iterable[(F, I)]): InvertedIndex[D, F, I] = {
            val docFeaturePairs = docs.iterator
                .map(doc => (doc, featureExtractor(doc)))
                .toSeq
            apply(docFeaturePairs: _*)
        }

    def combine[D, F, I](indices: Iterable[InvertedIndex[D, F, I]]): InvertedIndex[D, F, I] = {
        val combinedFeatures = indices.flatMap(_.index).groupMap(_._1)(_._2)
        val newIndex = combinedFeatures.view.mapValues(_.flatten.toSet).toMap
        new InvertedIndex(newIndex)
    }
}


class InvertedIndex[D, F, I] private (private val ind: Map[F, Set[(D, I)]]) {

    def ++(that: InvertedIndex[D, F, I]): InvertedIndex[D, F, I] = 
        InvertedIndex.combine(Iterable(this, that))

    def +:(element: (D, Iterable[(F, I)])): InvertedIndex[D, F, I] =
        this ++ InvertedIndex(element)

    def apply(feature: F, features: F*): Set[(D, I)] =
        if (features.isEmpty) {
            ind.getOrElse(feature, Set())
        } else {
            val feats = feature +: features
            feats.map(apply(_))
                .reduce(_ intersect _)
        }

    def updated(document: D, features: Iterable[(F, I)]): InvertedIndex[D, F, I] =
        (document, features) +: this

    override def equals(that: Any): Boolean =
        that match {
            case that: InvertedIndex[D, F, I] => {
                this.ind == that.ind
            }
            case _ => false
        }

    override def hashCode: Int = {
        ind.hashCode()
    }

  def features : Set[F] = ind.keySet
  def documents : Set[D] = ind.values.iterator.flatMap(_.map(_._1)).toSet
  def index : Map[F, Set[(D, I)]] = ind
}
