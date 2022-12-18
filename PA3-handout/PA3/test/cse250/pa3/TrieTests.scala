/**
 * cse250.pa3.TrieTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
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
package cse250.pa3

import org.scalatest._


class TrieTests extends FlatSpec with BeforeAndAfter {

  behavior of "Trie"
  it should "should insert multiple copies of mixed length strings properly (prefix free)" in {
    val testTrie = new Trie
    val words = List("aptitude12", "bsingleto", "cmultipl", "dapticu", "entowa", "ford1", "gmma", "hen", "ir", "j")
    val counts = List(15, 11,    22,    5,    7,    17,     9,     3,     4,    12)
    for ((word, count) <- words zip counts; _ <- 0 until count) {
      testTrie.insert(word)
    }

    assert(testTrie._storageGraph.vertices.length == 56)
    assert(testTrie._storageGraph.edges.length == 55)

    for ((word, count) <- words zip counts; _ <- 0 until count) {
      assert(testTrie.count(word) == count)
    }

    assert(testTrie.length == 105)

    val iter = testTrie.iterator
    while (iter.hasNext) println(iter.next)

    println(testTrie.allWordsOfLength(-1))
    println(testTrie.allWordsOfLength(0))
    println(testTrie.allWordsOfLength(1))
    println(testTrie.allWordsOfLength(2))
    println(testTrie.allWordsOfLength(3))
    println(testTrie.allWordsOfLength(4))

    println(testTrie.mostCommon(0))
    println(testTrie.mostCommon(-1))
    println(testTrie.mostCommon(5))
    println(testTrie.mostCommon(100))

    println(testTrie.mostCommonWithPrefix("t",5))
    println(testTrie.mostCommonWithPrefix("an",10))
    println(testTrie.mostCommonWithPrefix("cse",100))
    println(testTrie.mostCommonWithPrefix("",5))
    println(testTrie.mostCommonWithPrefix("",100))


    for (v <- testTrie._storageGraph.vertices) {
      println(v)
      testTrie._storageGraph.incidentEdges(v).foreach(println)
      println
    }
  }

  it should "should insert single copies of mixed length strings properly (prefix free)" in {
    val testTrie = new Trie
    val words = List("aptitude12", "bsingleto", "cmultipl", "dapticu", "entowa", "ford1", "gmma", "hen", "ir", "j")
    //    val words = List("ap", "an")
    for (word <- words) testTrie.insert(word)

    assert(testTrie._storageGraph.vertices.length == 56)
    assert(testTrie._storageGraph.edges.length == 55)
    for (v <- testTrie._storageGraph.vertices) {
      println(v)
      testTrie._storageGraph.incidentEdges(v).foreach(println)
      println
    }
  }

  it should "should insert multiple copies of length 10 strings properly (prefix free)" in {
    val testTrie = new Trie
    val words = List("aptitude12", "bsingleton", "cmultiplea", "dapticulat", "entoward64", "ford12stor", "gmmaculate", "hentative3", "iri5ent6ed", "jp23aniard")
    val counts = List(15, 11,    22,    5,    7,    17,     9,     3,     4,    12)
    for ((word, count) <- words zip counts; _ <- 0 until count) {
      testTrie.insert(word)
    }

    assert(testTrie._storageGraph.vertices.length == 101)
    assert(testTrie._storageGraph.edges.length == 100)
    for (v <- testTrie._storageGraph.vertices) {
      println(v)
      testTrie._storageGraph.incidentEdges(v).foreach(println)
      println
    }
  }

  it should "should insert single copies of length 10 strings properly (prefix free)" in {
    val testTrie = new Trie
    val words = List("aptitude12", "bsingleton", "cmultiplea", "dapticulat", "entoward64", "ford12stor", "gmmaculate", "hentative3", "iri5ent6ed", "jp23aniard")
//    val words = List("ap", "an")
    for (word <- words) testTrie.insert(word)

    assert(testTrie._storageGraph.vertices.length == 101)
    assert(testTrie._storageGraph.edges.length == 100)
    for (v <- testTrie._storageGraph.vertices) {
      println(v)
      testTrie._storageGraph.incidentEdges(v).foreach(println)
      println
    }
  }

  it should "should insert multiple copies of length 2 strings properly (prefix free)" in {
    val testTrie = new Trie
    val words = List("ap", "bp", "cn", "dn", "eo", "fd", "gm", "he", "id", "jn")
    val counts = List(15, 11,    22,    5,    7,    17,     9,     3,     4,    12)
    for ((word, count) <- words zip counts; _ <- 0 until count) {
      testTrie.insert(word)
    }

    assert(testTrie._storageGraph.vertices.length == 21)
    assert(testTrie._storageGraph.edges.length == 20)
    for (v <- testTrie._storageGraph.vertices) {
      println(v)
      testTrie._storageGraph.incidentEdges(v).foreach(println)
      println
    }
  }

  it should "should insert single copies of length 2 strings properly (prefix free)" in {
    val testTrie = new Trie
    val words = List("ap", "bp", "cn", "dn", "eo", "fd", "gm", "he", "id", "jn")
    for (word <- words) testTrie.insert(word)

    assert(testTrie._storageGraph.vertices.length == 21)
    assert(testTrie._storageGraph.edges.length == 20)
    for (v <- testTrie._storageGraph.vertices) {
      println(v)
      testTrie._storageGraph.incidentEdges(v).foreach(println)
      println
    }
  }

  it should "store the words from the given code" in {
    val testTrie = new Trie
    val words = List("a", "i", "an", "in", "to", "and", "inn", "tea", "ted", "ten")
    val counts = List(15, 11,    22,    5,    7,    17,     9,     3,     4,    12)
    for ((word, count) <- words zip counts; _ <- 0 until count) {
      testTrie.insert(word)
    }

    for ((word, count) <- words zip counts; _ <- 0 until count) {
      assert(testTrie.count(word) == count)
    }

    assert(testTrie.length == 105)

    val iter = testTrie.iterator
    while (iter.hasNext) println(iter.next)

    println(testTrie.allWordsOfLength(0))
    println(testTrie.allWordsOfLength(1))
    println(testTrie.allWordsOfLength(2))
    println(testTrie.allWordsOfLength(3))
    println(testTrie.allWordsOfLength(4))

    println(testTrie.mostCommon(20))

    println(testTrie.mostCommonWithPrefix("t",1))
    println(testTrie.mostCommonWithPrefix("a",1))
    println(testTrie.mostCommonWithPrefix("i",1))
    println(testTrie.mostCommonWithPrefix("to",1))
    println(testTrie.mostCommonWithPrefix("te",1))
    println(testTrie.mostCommonWithPrefix("",1))

  }

  it should "store the words from the assignment pdf doc" in {
    val testTrie = new Trie
    val words = List("a", "i", "an", "in", "to", "and", "inn", "tea", "ted", "ten", "")
    val counts = List(15,  7,    22,    3,    9,    17,     9,     3,     3,    12, 10)
    for ((word, count) <- words zip counts; _ <- 0 until count) {
      testTrie.insert(word)
    }

    for ((word, count) <- words zip counts; _ <- 0 until count) {
      assert(testTrie.count(word) == count)
    }

    assert(testTrie.length == 110)

    assert(testTrie._storageGraph.vertices.length == 13)
    assert(testTrie._storageGraph.edges.length == 12)

    val iter = testTrie.iterator
    while (iter.hasNext) println(iter.next)

    println(testTrie.allWordsOfLength(-1))
    println(testTrie.allWordsOfLength(0))
    println(testTrie.allWordsOfLength(1))
    println(testTrie.allWordsOfLength(2))
    println(testTrie.allWordsOfLength(3))
    println(testTrie.allWordsOfLength(4))

    println(testTrie.mostCommon(0))
    println(testTrie.mostCommon(-1))
    println(testTrie.mostCommon(5))
    println(testTrie.mostCommon(100))

    println(testTrie.mostCommonWithPrefix("t",5))
    println(testTrie.mostCommonWithPrefix("an",10))
    println(testTrie.mostCommonWithPrefix("cse",100))
    println(testTrie.mostCommonWithPrefix("",5))
    println(testTrie.mostCommonWithPrefix("",100))

  }

  behavior of "Trie"
  it should "store the words from text files" in {
    val testTrie = new Trie

    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile("data/2019-2020_Assessment_Roll.csv")
//    val inputFile = scala.io.Source.fromFile("data/ProfitingFromHedgeFunds.txt")
//    val inputFile = scala.io.Source.fromFile("data/SarasSideVerandah.txt")
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    var count = 0
    val lines = inputFile.getLines()
    for (line <- lines) {
      for (word <- line.split("[^a-zA-Z]")) {
        testTrie.insert(word)
        count = count + 1
      }
    }
    println("words stored: " + count)

    val iter = testTrie.iterator
    while (iter.hasNext) println(iter.next)

    println("The word 'hedge' occured " + testTrie.count("hedge") + " times.")
    println("Total number of words: " + testTrie.length)
    println("All words of length 8: " + testTrie.allWordsOfLength(8))
    println("20 most common words: " + testTrie.mostCommon(20))

  }

}

