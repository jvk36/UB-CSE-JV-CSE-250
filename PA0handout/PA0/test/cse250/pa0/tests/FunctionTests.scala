/**
 * FunctionTests.scala
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
package cse250.pa0.tests

import cse250.pa0.objects.Functions
import org.scalatest.FlatSpec

class FunctionTests extends FlatSpec {
  // Tests for problem 1.
  behavior of "FunctionsTest.genNum"

  it should "verify that the return value from Functions.genNum is even" in {
    for (i <- 0 to 1000)
      assert(Functions.genNum(i) % 2 == 0)
  }

  // Tests for problem 2.
  behavior of "FunctionsTest.genSeq"

  it should "verify that the sequence returned has n positive integer values" in {
    for (i <- 0 to 1000) {
      val ret = Functions.genSeq(i)
      assert(ret.length == i)
      for (j <- ret.indices) assert(ret(j) > 0)
    }
  }

  // Tests for problem 3.
  behavior of "FunctionsTest.funThree"

  it should "verify that Functions.funThree(i) is less than Functions.funThree(i+1) for all i" in {
    for (i <- 0 to 999)
      assert(Functions.funThree(i) < Functions.funThree(i+1))
  }

  // Tests for problem 4.
  behavior of "FunctionsTest.mapSum"

  def f1(x: Int) = { x + 1 }

  it should "verify that the return value is the sum of f1 applied to each of the values 1 thru n" in {
    var ret = 0
    for (i <- 0 to 1000) {
      if (i > 0) ret = ret + f1(i)
      assert(Functions.mapSum(i, f1) == ret)
    }
  }
}
