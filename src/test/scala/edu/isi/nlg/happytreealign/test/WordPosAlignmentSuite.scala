package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign.{NonEmptySpan, EmptySpan, WordPosAlignment}
import collection.immutable.SortedSet

class WordPosAlignmentSuite extends FunSuite {
  test("parse and construct") {
    val str = "0-0 0-1 0-2 1-2 2-0 10-2 10-3"
    val a = WordPosAlignment.parseFEPairs(str)
    assert(a.fToE === Map(0 -> SortedSet(0, 1, 2), 1 -> SortedSet(2), 2 -> SortedSet(0), 10 -> SortedSet(2, 3)))
    assert(a.eToF === Map(0 -> SortedSet(0, 2), 1 -> SortedSet(0), 2 -> SortedSet(0, 1, 10), 3 -> SortedSet(10)))
  }

  test("span mapping") {
    val a = WordPosAlignment.fromFEPairs(List(0 -> 0, 0 -> 1, 0 -> 2, 1 -> 2, 2 -> 0, 10 -> 2, 10 -> 3))

    assert(a.fSpanToESpan(EmptySpan) === EmptySpan)
    assert(a.eSpanToFSpan(EmptySpan) === EmptySpan)

    assert(a.fSpanToESpan(NonEmptySpan(0, 0)) === NonEmptySpan(0, 2))
    assert(a.fSpanToESpan(NonEmptySpan(0, 1)) === NonEmptySpan(0, 2))
    assert(a.fSpanToESpan(NonEmptySpan(0, 2)) === NonEmptySpan(0, 2))
    assert(a.fSpanToESpan(NonEmptySpan(1, 1)) === NonEmptySpan(2, 2))
    assert(a.fSpanToESpan(NonEmptySpan(2, 2)) === NonEmptySpan(0, 0))
    assert(a.fSpanToESpan(NonEmptySpan(2, 5)) === NonEmptySpan(0, 0))
    assert(a.fSpanToESpan(NonEmptySpan(0, 3)) === NonEmptySpan(0, 2))
    assert(a.fSpanToESpan(NonEmptySpan(0, 5)) === NonEmptySpan(0, 2))
    assert(a.fSpanToESpan(NonEmptySpan(0, 10)) === NonEmptySpan(0, 3))
    assert(a.fSpanToESpan(NonEmptySpan(1, 10)) === NonEmptySpan(0, 3))
    assert(a.fSpanToESpan(NonEmptySpan(10, 11)) === NonEmptySpan(2, 3))
    assert(a.fSpanToESpan(NonEmptySpan(5, 9)) === EmptySpan)

    assert(a.eSpanToFSpan(NonEmptySpan(0, 0)) === NonEmptySpan(0, 2))
    assert(a.eSpanToFSpan(NonEmptySpan(0, 1)) === NonEmptySpan(0, 2))
    assert(a.eSpanToFSpan(NonEmptySpan(1, 1)) === NonEmptySpan(0, 0))
    assert(a.eSpanToFSpan(NonEmptySpan(0, 2)) === NonEmptySpan(0, 10))
    assert(a.eSpanToFSpan(NonEmptySpan(0, 3)) === NonEmptySpan(0, 10))
    assert(a.eSpanToFSpan(NonEmptySpan(0, 5)) === NonEmptySpan(0, 10))
    assert(a.eSpanToFSpan(NonEmptySpan(2, 5)) === NonEmptySpan(0, 10))
    assert(a.eSpanToFSpan(NonEmptySpan(3, 5)) === NonEmptySpan(10, 10))
    assert(a.eSpanToFSpan(NonEmptySpan(4, 5)) === EmptySpan)
  }
}
