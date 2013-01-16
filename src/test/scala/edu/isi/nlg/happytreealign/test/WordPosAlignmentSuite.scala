package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign.WordPosAlignment
import collection.immutable.SortedSet

class WordPosAlignmentSuite extends FunSuite {
  test("parse and construct") {
    val str = "0-0 0-1 0-2 1-2 10-2 10-3"
    val a = WordPosAlignment.parseFEPairs(str)
    assert(a.fToE === Map(0 -> SortedSet(0, 1, 2), 1 -> SortedSet(2), 10 -> SortedSet(2, 3)))
    assert(a.eToF === Map(0 -> SortedSet(0), 1 -> SortedSet(0), 2 -> SortedSet(0, 1, 10), 3 -> SortedSet(10)))
  }
}
