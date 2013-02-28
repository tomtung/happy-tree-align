package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import com.typesafe.scalalogging.slf4j.Logging

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

object AlignmentTree extends Logging {
  def parseAlignmentTrees(syntaxTreeFileName: String, alignmentFileName: String): Iterator[AlignmentTree] =
    parseAlignmentTreesOp(syntaxTreeFileName, alignmentFileName).flatten

  def parseAlignmentTreesOp(syntaxTreeFileName: String, alignmentFileName: String): Iterator[Option[AlignmentTree]] = {
    def syntaxTreeLines =
      io.Source.fromFile(syntaxTreeFileName, "UTF-8").getLines().map(_.trim)
    def alignmentLines =
      io.Source.fromFile(alignmentFileName, "UTF-8").getLines().map(_.trim)

    if (syntaxTreeLines.length != alignmentLines.length)
      throw new ParsingException("Line number different between syntax tree file and word alignment file")

    (syntaxTreeLines zip alignmentLines) map {
      case (treeLine, alignLine) =>
        if (treeLine.isEmpty != alignLine.isEmpty) {
          if (treeLine.isEmpty) {
            logger.warn("Alignment line paired with blank syntax tree line: " + alignLine)
          }
          if (alignLine.isEmpty) {
            logger.warn("Syntax tree line paired with blank alignment line: " + treeLine)
          }

          None
        } else Some {
          val tree = SyntaxTree.parse(treeLine)
          val align = WordPosAlignment.parseFEPairs(alignLine)
          new AlignmentTree(tree, align)
        }
    }
  }
}
