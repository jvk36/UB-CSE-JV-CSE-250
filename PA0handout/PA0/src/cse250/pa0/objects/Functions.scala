/**
 * Functions.scala
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
package cse250.pa0.objects

import scala.util.Random

object Functions {
  def genNum(n: Int): Int = {
    // correct version,
    n * 2
    // incorrect version
//    n / 2
  }

  def genSeq(n: Int): Seq[Int] = {
    // Seq[Int](0)
    // correct version
    if (n==0)  Seq[Int]()
    else {
      for (i <- 1 to n) yield i
    }
    // incorrect version
//    for (i <- 0 to n) yield i
  }

  def funThree(n: Int): Int = {
    // correct version
    n
    // incorrect version
//    if (n>1) 0 else 1
  }

  def mapSum(n: Int, f: Int => Int): Int = {
    var ret = 0
    // incorrect version
    // for (i <- 0 to n) {
    // correct version
    for (i <- 1 to n) {
      ret = ret + f(i)
    }
    ret
  }
}
