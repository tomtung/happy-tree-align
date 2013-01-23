package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import trans._
import com.typesafe.scalalogging.slf4j.Logging

trait TransformationExtractor {
  def extract(tree: SyntaxTree): Set[Transformation] = {
    tree.traverseLeftRightBottomUp.iterator.
      flatMap(extractAtAnchorNode).
      toSet
  }

  def extract(alignTree: AlignmentTree): Set[Transformation] =
    extract(alignTree.syntaxTree).filter(
      trans => trans(alignTree).agreementScore > alignTree.agreementScore)

  protected def extractAtAnchorNode(node: Node): TraversableOnce[Transformation]
}

object TransformationExtractor extends TransformationExtractor with Logging {
  val extractors = List(Articulate, Flatten, Promote, Demote, Transfer, Adopt)

  protected def extractAtAnchorNode(node: Node): TraversableOnce[Transformation] =
    extractors.iterator.flatMap(_.extractAtAnchorNode(node))

  def findBestTransformation(alignTrees: Vector[AlignmentTree]): Option[(Transformation, Vector[AlignmentTree], Int)] =
    alignTrees.par.map(extract).reduceOption(_ union _).map(candidateTrans => {
      logger.trace("size of candidate transformation set: " + candidateTrans.size)
      candidateTrans.par.map(
        trans => {
          val transformedTrees = alignTrees.map(trans(_))
          val totalScore = transformedTrees.iterator.map(_.agreementScore).sum
          (trans, transformedTrees, totalScore)
        }).maxBy(_._3)
    })
}
