package cse250.pa1

import cse250.objects.{AssessmentUtilities, TaxParcel}
import org.scalatest.{BeforeAndAfter, FlatSpec}


class DataEntryStoreTests extends FlatSpec with BeforeAndAfter {
  var dataStore: DataEntryStore[TaxParcel] = _

  before {
    // This code will execute before each test.
    dataStore = new DataEntryStore[TaxParcel](AssessmentUtilities.maxCapacity)
  }

  behavior of "DataEntryStore.dataArray"
  it should "should be initialized to all empty nodes" in {
    for(entry <- dataStore.dataArray)
      assert(entry == dataStore.emptyNode)
  }

  behavior of "DataEntryStore.head and DataEntryStore.tail"
  it should "always be initialized to -1" in {
    assert(dataStore.headIndex == -1)
    assert(dataStore.tailIndex == -1)
  }

  behavior of "DataEntryStore.length"
  it should "always be initialized to 0" in {
    assert(dataStore.length == 0)
  }

  behavior of "DataEntryStore.insert"
  it should "verify an entry is in the data entry store after each insert" in {
    val entries1 = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries1.length) {
      dataStore.insert(entries1(i))
      assert(dataStore.length == i + 1)
    }
  }
  it should "verify that the inserted taxParcel is at the end of the list" in {
    val entries1 = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries1.length) {
      dataStore.insert(entries1(i))
      assert(dataStore.tailIndex != -1)
      assert(dataStore(dataStore.tailIndex) == entries1(i))
    }
  }
  it should "not increase numStored once capacity is reached" in {
    val entries1 = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries1.length) {
      dataStore.insert(entries1(i))
    }
    var num = dataStore.length
    dataStore.insert(entries1(0))
    assert(num == dataStore.length)
  }

  it should "overwrite the oldestEntry once the capacity is reached and the inserted node should still be the newest entry (tail)" in {
    val entries1 = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries1.length) {
      dataStore.insert(entries1(i))
    }
    var tempHeadIndex = dataStore.headIndex
    dataStore.insert(entries1(1))
    assert(dataStore(tempHeadIndex) == entries1(1))
    assert(dataStore(dataStore.tailIndex) == entries1(1))
  }

  it should "allow duplicate entries" in {
    val entries1 = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (_ <- 0 until entries1.length) {
      dataStore.insert(entries1(1))
    }
    assert(dataStore(dataStore.headIndex) == dataStore(dataStore.tailIndex))
    assert(dataStore.length == entries1.length)
  }

  behavior of "DataEntryStore.remove"
  it should "check that the nodes are removed and restored to empty node" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    for (i <- 0 until entries.length) {
      dataStore.remove(entries(i))
    }
    assert(dataStore.length == 0)
    for (elem <- dataStore.dataArray) {
      assert(elem == dataStore.emptyNode)
    }
  }

  it should "verify that the function returns true and false correctly" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 1 until entries.length) {
      dataStore.insert(entries(i))
    }
    assert(dataStore.remove(entries(2)) == true)
    assert(dataStore.remove(entries(0)) == false)
  }

  it should "verify that the function removes duplicate entries correctly" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      if (i >= 2) {
        dataStore.insert(entries(2))
      }
      else {
        dataStore.insert(entries(i))
      }
    }
    assert(dataStore.length == entries.length)
    dataStore.remove(entries(2))
    assert(dataStore.length == 2)
  }

  behavior of "DataEntryStore.countEntry"
  it should "return 1 if there is an entry but it is not duplicated" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      assert(dataStore.countEntry(entries(i)) == 1)
    }
  }

  it should "return 0 if the entry is not present" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    dataStore.remove(entries(5))
    assert(dataStore.countEntry(entries(5)) == 0)
  }

  behavior of "DataEntryStore.countEntry"
  it should "return correct count if there are duplicated entries" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      if (i >= 2) {
        dataStore.insert(entries(2))
      }
      else {
        dataStore.insert(entries(i))
      }
    }
    assert(dataStore.countEntry(entries(2)) == dataStore.length - 2)
  }

  behavior of "DataEntryStore.apply"
  it should "return the entry at given index from that sequence" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
      assert(dataStore.apply(i) == entries(i))
    }
  }

  it should "return the correct node if the insertions have passed capacity" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    dataStore.insert(entries(6))
    assert(dataStore.apply(dataStore.length - 1) == entries(6))
  }

  behavior of "DataEntryStore.update"
  it should "update the entry at given index from that sequence" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)
    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }
    dataStore.update(5, entries(7))
    assert(dataStore(5) == entries(7))
  }

  behavior of "DataEntryStore.iterator"
  it should "return an iterator that can be used to traverse the list" in {
    val entries = AssessmentUtilities.loadAssessmentEntries(AssessmentUtilities.filename, AssessmentUtilities.maxCapacity)

    for (i <- 0 until entries.length) {
      dataStore.insert(entries(i))
    }

    var i=0
    for (item <- dataStore.iterator) {
      assert(item == entries(i))
      i = i+1
    }
  }
}
