/**
 * Main.scala
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

object Main {
  def main(args: Array[String]): Unit = {
//    val filename = "data/2019-2020_Assessment_Roll.csv"
    val filename = "data/2019-2020_Assessment_Roll_10.csv"
    AssessmentDataProcessor.sanitizeData(filename)
    println(AssessmentDataProcessor.computeMostExpensiveEntry(filename + "-updated"))
    println(AssessmentDataProcessor.countPriceRange(filename + "-updated", 1, 10000))
  }
}
