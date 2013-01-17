package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node

class AlignmentTree(val syntaxTree: SyntaxTree, val alignment: WordPosAlignment) {
  lazy val agreementScore: Int = {
    val (pos, neg) = syntaxTree.traverseBreadthFirst.iterator.map(eSpanOf).filter(_.length > 1).toSet.partition(isExtractable)
    pos.size - neg.size
  }

  def eSpanOf(node: Node): Span = syntaxTree.spanOf(node)

  def fSpanOf(node: Node): Span = alignment.eSpanToFSpan(eSpanOf(node))

  def isExtractable(eSpan: Span): Boolean =
    eSpan contains alignment.fSpanToESpan(alignment.eSpanToFSpan(eSpan))
}
