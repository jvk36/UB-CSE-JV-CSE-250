/**
 * cse250.pa3.Trie.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: annkonna
 * Person#: 50253772
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa3

import cse250.examples.adaptors.LectureQueueSinglyLinkedList
import cse250.examples.graph.AdjacencyListGraph

import scala.collection.mutable

class Trie {
  /** The graph to store the trie. */
  val _storageGraph = new AdjacencyListGraph[Int,Char]
  /** The root of the trie. */
  val _trieRoot = _storageGraph.insertVertex(0)

  /** get the children */
  def childEdges (node: _storageGraph.Vertex, parNode: _storageGraph.Vertex): Iterator[_storageGraph.Edge] = {
    var seq: List[_storageGraph.Edge]  = List()
    for (e <- _storageGraph.incidentEdges(node)) {
      if (e.opposite(node) != parNode) {
        seq = seq.appended(e)
      }
    }
    seq.iterator
  }

  /** Inserts the given word into the trie graph. */
  def insert(word: String): Unit = {
    var curNode = _trieRoot
    var parNode: _storageGraph.Vertex = null
    for (l <- word) {
      var found = false
      val children = childEdges(curNode, parNode)
      while (children.hasNext && !found){
        val e = children.next()
        parNode = curNode
        if (e.elem == l) {
          curNode = e.opposite(curNode)
          found = true
        }
      }
      if (!found) {
        val newVertex = _storageGraph.insertVertex(0)
        _storageGraph.insertEdge(curNode, newVertex, l)
        parNode = curNode
        curNode = newVertex
      }
    }
    curNode._elem = curNode.elem + 1
  }

  /** Returns the number of times the given word was inserted. */
  def count(word: String): Int = {
    var curNode = _trieRoot
    var parNode: _storageGraph.Vertex = null
    for (l <- word) {
      var found = false
      val children = childEdges(curNode, parNode)
      while (children.hasNext && !found) {
        val e = children.next()
        parNode = curNode
        if (e.elem == l) {
          curNode = e.opposite(curNode)
          found = true
        }
      }
      if (!found) {
        return 0
      }
    }
    curNode.elem
  }

  /** Returns the number of words stored within. */
  def length: Int = {
    var count = 0
    for (v <- _storageGraph.vertices) {
      count = count + v.elem
    }
    count
  }

  /** Returns an Iterator that can be used only once. */
  def iterator: Iterator[String] = new Iterator[String] {
    val queue = new LectureQueueSinglyLinkedList[(_storageGraph.Vertex, _storageGraph.Vertex, String)]()
    var children = childEdges(_trieRoot, null)
    queue.enqueue(_trieRoot, null, "")
    var res: (_storageGraph.Vertex, _storageGraph.Vertex, String) = null
    var found = _trieRoot.elem > 0
    var curWord = {
      var edge: _storageGraph.Edge = null
      while (!queue.isEmpty && !found) {
        while (children.hasNext && !found) {
          edge = children.next()
          if (edge.opposite(queue.front._1)._elem> 0) {
            found = true
          }
          queue.enqueue(edge.opposite(queue.front._1), queue.front._1, queue.front._3 + edge._elem)
        }
        if (!found) {
          queue.dequeue
          children = childEdges(queue.front._1, queue.front._2)
        }
      }
      if (found) {
        if (edge==null) queue.front._3 else queue.front._3 + edge._elem
      } else ""
    }

    override def hasNext: Boolean = found

    override def next: String = {
      val ret = curWord

      found = false
      var edge: _storageGraph.Edge = null
      while (!queue.isEmpty && !found) {
        while (children.hasNext && !found) {
          edge = children.next()
          if (edge.opposite(queue.front._1)._elem> 0) {
            found = true
          }
          queue.enqueue(edge.opposite(queue.front._1), queue.front._1, queue.front._3 + edge._elem)
        }
        if (!found) {
          queue.dequeue
          if (!queue.isEmpty) {
            children = childEdges(queue.front._1, queue.front._2)
          }
        }
      }

      curWord = if (found) queue.front._3 + edge._elem else ""

      ret
    }
  }

  /** Returns a sequence of all words of a given length ordered alphabetically. */
  def allWordsOfLength(length: Int): Seq[String] = {
    object WordOrder extends Ordering[String] {
      def compare(a:String, b:String) = b compare a
    }
    val priorityQueue: mutable.PriorityQueue[String] =
      mutable.PriorityQueue()(WordOrder)
    val iter = iterator
    while (iter.hasNext) {
      val word = iter.next()
      if (word.length == length) {
        priorityQueue.enqueue(word)
      }
    }
    priorityQueue.dequeueAll
  }

  /** Returns a sequence containing the k most inserted words.*/
  def mostCommon(k: Int): Seq[String] = {
    mostCommonWithPrefix("", k)
  }

  /** Returns a sequence containing the k most inserted words that start with the given prefix. */
  def mostCommonWithPrefix(prefix: String, k: Int): Seq[String] = {
    val priorityQueue: mutable.PriorityQueue[(String, Int)] =
      mutable.PriorityQueue()(Ordering[(Int, String)].on( x => (x._2, x._1)))

    val iter = iterator
    while (iter.hasNext) {
      val word = iter.next()
      val numOcc = count(word)
      if (word.startsWith(prefix)) {
        priorityQueue.enqueue((word, numOcc))
      }
    }

    priorityQueue.dequeueAll.take(k).reverse.map(k => k._1)
  }
}

