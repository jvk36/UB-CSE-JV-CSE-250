/**
 * Main.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Modify at your leisure, but this will not be graded.
 */
package cse250.pa1

import cse250.objects.{AssessmentUtilities, TaxParcel}

object Main {
  def main(args: Array[String]): Unit = {
    val taxEntryStore = new DataEntryStore[TaxParcel](10)
    val filename = "data/2019-2020_Assessment_Roll-updated.csv"

    var numLines = 5
    for (entry <- AssessmentUtilities.loadAssessmentEntries(filename, numLines)) {
      taxEntryStore.insert(entry)
    }
    println(s"Storage after $numLines additions:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 10
    for (entry <- AssessmentUtilities.loadAssessmentEntries(filename, numLines)) {
      taxEntryStore.insert(entry)
    }
    println(s"Storage after $numLines more additions:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 5
    for (entry <- AssessmentUtilities.loadAssessmentEntries(filename, numLines)) {
      taxEntryStore.remove(entry)
    }
    println(s"Storage after removal of first $numLines lines:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 10
    for (entry <- AssessmentUtilities.loadAssessmentEntries(filename, numLines)) {
      taxEntryStore.remove(entry)
    }
    println(s"Storage after removal of first $numLines lines:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 1
    for (entry <- AssessmentUtilities.loadAssessmentEntries(filename, numLines); _ <- 1 to 5) {
      taxEntryStore.insert(entry)
    }
    println(s"Storage after adding 5 copies of of first line:")
    println("-----")
    println(taxEntryStore)
    println("-----")

    numLines = 1
    for (entry <- AssessmentUtilities.loadAssessmentEntries(filename, numLines)) {
      taxEntryStore.remove(entry)
    }
    println(s"Storage after removing first line:")
    println("-----")
    println(taxEntryStore)
    println("-----")
  }
}
