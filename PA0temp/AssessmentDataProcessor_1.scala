/**
 * TaxEntryProcessor.scala
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

import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter

import cse250.objects.TaxParcel

import scala.io.Source

object AssessmentDataProcessor {
  def sanitizeData(filename: String): Unit = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()

    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))

    for (line <- lines) {
//      var colsIter = """[^\",]*,?|(\"[^\"]+\")+,?""".r.findAllIn(line)
      var colsIter = """,|[^\",]+,?|(\"[^\"]+\")+,?""".r.findAllIn(line)

      var flag = true
      var i=0
      var newLine = ""
      for (col <- colsIter) {
        // Remove row if the zipcode column (column #20) is empty
        if (i == 19 && col == ",") {
          flag = false
        }
        // Remove columns 9 to 14, 21 to 25, 32 to 34, and 40 to 42 - 29 columns should remain for every row.
        if (!Array(8, 9, 10, 11, 12, 13, 20, 21, 22, 23, 24, 31, 32, 33, 39, 40, 41).contains(i))
          newLine = newLine + col
        i = i + 1
      }

//      println(i)

      if (flag) {
        outputFile.write(newLine)
        outputFile.write('\n')
      }
    }

    // Close the files at the end.
    inputFile.close()
    outputFile.close()
  }

  def computeMostExpensiveEntry(filename: String): TaxParcel = {
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()
    var mostExpensive:Double = 0
    var retParcel = new TaxParcel
    for (line <- lines) {
      var cols = line.split(""",""", -1)
//      println(cols.length)
      var ret:Double = cols(15).toDoubleOption.getOrElse(0)
      if (ret > mostExpensive) {
        mostExpensive = ret
        for (i <- 0 to 28) {
          retParcel.parcelInfo(TaxParcel.HEADERS(i)) = cols(i)
        }
      }
    }

    println("Most Expensive is valued at " + mostExpensive)

    retParcel
  }

  def countPriceRange(filename: String, lower: Int, upper: Int): Int = {
    var count=0
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()
    var mostExpensive:Int = 0
    var retParcel = new TaxParcel
    for (line <- lines) {
      var cols = line.split(""",""", -1)
      //      println(cols.length)
      var ret:Int = cols(15).toIntOption.getOrElse(0)
      if (ret >= lower && ret <= upper) {
        count = count + 1
      }
    }

    count
  }
}
