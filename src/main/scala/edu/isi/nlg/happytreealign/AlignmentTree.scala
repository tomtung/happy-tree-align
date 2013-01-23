package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node

class AlignmentTree(val syntaxTree: SyntaxTree, val alignment: WordPosAlignment) {
  lazy val agreementScore: Int = {
    val (pos, neg) = syntaxTree.traverseLeftRightBottomUp.iterator.map(eSpanOf).filter(_.length > 1).toSet.partition(isExtractable)
    pos.size - neg.size
  }

  def eSpanOf(node: Node): Span = syntaxTree.spanOf(node)

  def fSpanOf(node: Node): Span = alignment.eSpanToFSpan(eSpanOf(node))

  def isExtractable(eSpan: Span): Boolean =
    eSpan contains alignment.fSpanToESpan(alignment.eSpanToFSpan(eSpan))

  lazy val transformationToScoreDiff: Map[Transformation, Int] =
    TransformationExtractor.extract(syntaxTree).iterator.
      map(trans => trans -> (trans(this).agreementScore - agreementScore)).
      filter(_._2 != 0).
      toMap
}

object AlignmentTree {
  def parseAlignmentTrees(syntaxTreeFile: String, alignmentFile: String): Iterator[AlignmentTree] = {
    def syntaxTreeLines =
      io.Source.fromFile(syntaxTreeFile, "UTF-8").getLines()
    def alignmentLines =
      io.Source.fromFile(alignmentFile, "UTF-8").getLines()

    if (syntaxTreeLines.length != alignmentLines.length)
      throw new ParsingException("Different number of syntax trees and word alignments")

    (syntaxTreeLines.map(SyntaxTree.parse) zip alignmentLines.map(WordPosAlignment.parseFEPairs)).map({
      case (tree, align) => new AlignmentTree(tree, align)
    })
  }
}
