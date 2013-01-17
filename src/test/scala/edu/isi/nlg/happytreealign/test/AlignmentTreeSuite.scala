package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign._

class AlignmentTreeSuite extends FunSuite {
  test("node span (both (F to E and vice versa))") {
    val syntaxTree = SyntaxTree.parse("( (FRAG (NP (NN xinhua) (NN news) (NN agency)) (, ,) (NP (FW hong) (FW kong)) (, ,) (NP (NN february) (NN 23) (NN rd))) )")
    val alignment = WordPosAlignment.parseFEPairs("0-0 0-1 0-2 1-4 1-5 2-7 3-8 4-9")
    val alignmentTree = new AlignmentTree(syntaxTree, alignment)

    assert(alignmentTree.syntaxTree.traverseBreadthFirst.map(alignmentTree.eSpanOf) ===
      List(
        Span(0, 9),
        Span(0, 2), Span(3, 3), Span(4, 5), Span(6, 6), Span(7, 9),
        Span(0, 0), Span(1, 1), Span(2, 2), Span(3, 3), Span(4, 4), Span(5, 5), Span(6, 6), Span(7, 7), Span(8, 8), Span(9, 9),
        Span(0, 0), Span(1, 1), Span(2, 2), Span(4, 4), Span(5, 5), Span(7, 7), Span(8, 8), Span(9, 9)))

    assert(alignmentTree.syntaxTree.traverseBreadthFirst.map(alignmentTree.fSpanOf) ===
      List(
        Span(0, 4),
        Span(0, 0), EmptySpan, Span(1, 1), EmptySpan, Span(2, 4),
        Span(0, 0), Span(0, 0), Span(0, 0), EmptySpan, Span(1, 1), Span(1, 1), EmptySpan, Span(2, 2), Span(3, 3), Span(4, 4),
        Span(0, 0), Span(0, 0), Span(0, 0), Span(1, 1), Span(1, 1), Span(2, 2), Span(3, 3), Span(4, 4)))
  }

  test("e-span extractability") {
    val syntaxTree = SyntaxTree.parse("( (FRAG (NP (NN xinhua) (NN news) (NN agency)) (, ,) (NP (FW hong) (FW kong)) (, ,) (NP (NN february) (NN 23) (NN rd))) )")
    val alignment = WordPosAlignment.parseFEPairs("0-0 0-1 0-2 1-4 1-5 2-7 3-8 4-9")
    val alignmentTree = new AlignmentTree(syntaxTree, alignment)

    assert(alignmentTree.isExtractable(EmptySpan))
    assert(alignmentTree.isExtractable(Span(0, 2)))
    assert(alignmentTree.isExtractable(Span(0, 3)))
    assert(alignmentTree.isExtractable(Span(3, 5)))
    assert(alignmentTree.isExtractable(Span(4, 5)))
    assert(alignmentTree.isExtractable(Span(3, 6)))
    assert(alignmentTree.isExtractable(Span(6, 9)))
    assert(alignmentTree.isExtractable(Span(0, 9)))
    assert(!alignmentTree.isExtractable(Span(0, 0)))
    assert(!alignmentTree.isExtractable(Span(0, 1)))
    assert(!alignmentTree.isExtractable(Span(1, 2)))
    assert(!alignmentTree.isExtractable(Span(3, 4)))
    assert(!alignmentTree.isExtractable(Span(2, 5)))
    assert(!alignmentTree.isExtractable(Span(1, 9)))
  }

  test("agreement score") {
    val syntaxTree1 = SyntaxTree.parse("( (S (NP The first step) (VP (VBZ is) (S (VP (TO to) (VP (VB select) (ADVP team members)))))) )")
    val syntaxTree2 = SyntaxTree.parse("( (S (NP The first step) (VP (VBZ is) (S (VP (TO+VB (TO to) (VB select)) (VP (ADVP team members)))))) )")
    val alignment = WordPosAlignment.fromFEPairs(List(0 -> 1, 1 -> 0, 1 -> 2, 2 -> 3, 3 -> 4, 3 -> 5, 4 -> 6, 4 -> 7))
    assert(new AlignmentTree(syntaxTree2, alignment).agreementScore === 6)
    assert(new AlignmentTree(syntaxTree1, alignment).agreementScore === 4)
  }
}
