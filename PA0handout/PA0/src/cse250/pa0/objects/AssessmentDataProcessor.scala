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

  def getColumnsRegex(line: String): Array[String] = {

/*    The regular expression has 3 components that are Ored using | operator.
    1.	The first one comma (,) matches the comma character.
    2.	The second one [^\",]+,? Matches a character sequence that is not a double quote or
        comma one or more times followed by a comma or nothing.
    3.	The last one (\"[^\"]+\")+,? matches a double quote followed by more than one non-double-quote characters
        followed by a double quote. That sequence of characters is repeated one or more times. That is followed
        by a comma or nothing.
*/

    var ret = """,|[^\",]+,?|(\"[^\"]+\")+,?|(\"\"\"[^\"]+\"\"\")+,?""".r.findAllIn(line).toArray
    for (i <- ret.indices) {
      if (ret(i).endsWith(",")) {
        ret(i) = ret(i).stripSuffix(",")
      }
    }
    if (ret.length == 28) {
      ret = ret:+""
    }
    ret
  }

  def getColumns(line: String): Array[String] = {
    val len = line.length
    var start = 0
    var cur = 0
    var ret:Array[String] = new Array(0)

    var flag = false
    while (cur != len) {
      if ( line(cur) == ',' && !flag) {
        ret = ret :+ line.substring(start, cur)
        cur = cur+1
        start = cur
      } else if ( line(cur) == '"') {
        flag = !flag
        cur = cur+1
      } else {
        cur = cur + 1
      }
    }

    if (ret.length == 28) {
      ret = ret:+""
    }

    ret
  }

  def sanitizeData(filename: String): Unit = {
    // For opening files, look at Scala Cookbook File I/O Excerpt
    val inputFile = scala.io.Source.fromFile(filename)
    // Note: lines is an iterator to the file. This is only valid as long as the file is open.
    //       Ensure you do not close the file prior to finishing the file usage.
    val lines = inputFile.getLines()

    val outputFile = new BufferedWriter(new FileWriter( new File(filename + "-updated")))

    for (line <- lines) {
      var cols = getColumns(line)
      var flag = true
      var newLine = ""
      for (i <- cols.indices) {
        // Remove row if the zipcode column (column #20) is empty
        if (i == 19 && cols(i).isEmpty()) {
          flag = false
        }
        // Remove columns 9 to 14, 21 to 25, 32 to 34, and 40 to 42 - 29 columns should remain for every row.
        if (!Array(8, 9, 10, 11, 12, 13, 20, 21, 22, 23, 24, 31, 32, 33, 39, 40, 41).contains(i))
          newLine = newLine + cols(i) + ","
      }

//      println(cols.length)

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
    var mostExpensive:Int = 0
    var retParcel = new TaxParcel
    for (line <- lines) {
      var cols = getColumns(line)
      assert(cols.length==29)
      // get the the 16th column (TOTAL VALUE) value
      var ret:Int = cols(15).toIntOption.getOrElse(0)
      if (ret > mostExpensive) {
        mostExpensive = ret
        for (i <- cols.indices) {
          /* replace two consecutive double-quotes with a single double-quote and one double-quote with empty string */
          // the regex matches double-quotes repeated 2 or more times. replaceAll replaces it with "&#".
          cols(i) = cols(i).replaceAll("\"{2,}", "&#")
          // the regex matches double-quotes or nothing. replaceAll replaces it with empty-string.
          cols(i) = cols(i).replaceAll("\"?", "")
          // the regex matches the string "&#". replaceAll replaces it with double-quote.
          cols(i) = cols(i).replaceAll("&#", "\"")
//          cols(i) = cols(i).replaceAll("\"", "")
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
      var cols = getColumns(line)
      //      println(cols.length)
      var ret:Int = cols(15).toIntOption.getOrElse(0)
      if (ret >= lower && ret <= upper) {
        count = count + 1
      }
    }

    count
  }
}
