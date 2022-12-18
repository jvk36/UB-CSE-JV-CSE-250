/**
 * cse250.pa5.HashTableMap.scala
 *
 * Copyright 2019 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT:
 * Person#:
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa4

import cse250.examples.types.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V](_alphaMax: Double = 0.6)(implicit hash: Hashing[K]) extends Map[K, V] {
  var _n = 0
  var _N = 10
  var _alpha: Double = 0.0
  var _bucketArray = Array.fill[ListBuffer[(K, V)]](_N)(ListBuffer[(K, V)]())

  def rehash(newSize: Int): Unit = {
    if (newSize > _N) {
      val oldBucketArray = _bucketArray
      _n = 0
      _N = newSize
      _alpha = 0.0
      _bucketArray = Array.fill(_N)(ListBuffer[(K, V)]())
      for (bucket <- oldBucketArray; elem <- bucket) addOne(elem)
    }
  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % _N
    _bucketArray(lookupIndex).find(elem => elem._1 == key) match {
      case Some(elem) => Some(elem._2)
      case None       => None
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    var bucketIndex = hash.hash(elem._1) % _N
    val listIndex = _bucketArray(bucketIndex).indexWhere(pair => pair._1 == elem._1)
    if (listIndex != -1) {
      _bucketArray(bucketIndex)(listIndex) = elem
    } else {
      if ((_n+1.0)/_N > _alphaMax) {
        rehash(_N * 2)
        bucketIndex = hash.hash(elem._1) % _N
      }
      _bucketArray(bucketIndex).prepend(elem)
      _n = _n+1
      _alpha = 1.0 * _n/_N
    }
  }

  override def removeOne(key: K): Boolean = {
    val bucketIndex = hash.hash(key) % _N
    val listIndex = _bucketArray(bucketIndex).indexWhere(pair => pair._1 == key)
    if (listIndex == -1) return false
    _bucketArray(bucketIndex).remove(listIndex)
    _n = _n - 1
    _alpha = 1.0 * _n/_N
    true
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    var bucketIndex = _bucketArray.indexWhere(k => k.nonEmpty)
    var bucketPos = {
      if (bucketIndex == -1) null
      else _bucketArray(bucketIndex).iterator
    }

    def hasNext: Boolean = (bucketPos != null) && bucketPos.hasNext

    def next(): (K, V) = {
      val ret = bucketPos.next

      if (!bucketPos.hasNext) {
        bucketIndex = _bucketArray.indexWhere(k => k.nonEmpty, bucketIndex+1)
        if (bucketIndex != -1) bucketPos = _bucketArray(bucketIndex).iterator
      }

      ret
    }
  }
}
