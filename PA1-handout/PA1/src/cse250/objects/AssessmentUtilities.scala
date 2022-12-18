/**
 * cse250.objects.AssessmentUtilities.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * DO NOT MODIFY THIS FILE
 */
package cse250.objects

import scala.collection.mutable

object AssessmentUtilities {
  val filename = "data/2019-2020_Assessment_Roll-updated.csv"
  val maxCapacity = 10

  val csvRegex = """((?:"(?:(?:"")|[^"])*")|[^,"]*),?""".r

  /**
   * Loads the assessment data from a file that was processed into an
   * array buffer.
   *
   * @param filename              - path to updated assessment data file.
   * @param numberOfEntriesToLoad - number of TaxParcel entries to load.
   * @return an ArrayBuffer[TaxParcel] holding the first numberOfEntriesToLoad values.
   */
  def loadAssessmentEntries(filename: String, numberOfEntriesToLoad: Int) = {
    // Scala Cookbook reading CSV:
    // https://www.oreilly.com/library/view/scala-cookbook/9781449340292/ch12s06.html
    val dataSource = io.Source.fromFile(filename)
    val lines = dataSource.getLines
    val buffer = new mutable.ArrayBuffer[TaxParcel]
    lines.next // Skip header.
    var lineCount = 0
    for (line <- lines; if lineCount < numberOfEntriesToLoad) {
      lineCount += 1
      val parcel = new TaxParcel
      for ((m, col) <- csvRegex.findAllMatchIn(line) zip (0 to 28))
        parcel.parcelInfo(TaxParcel.HEADERS(col)) = m.group(1)

      buffer.append(parcel)
    }
    dataSource.close
    buffer
  }
}
