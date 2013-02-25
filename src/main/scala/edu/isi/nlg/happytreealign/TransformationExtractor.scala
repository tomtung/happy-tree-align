package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import trans._
import com.typesafe.scalalogging.slf4j.Logging
import collection.parallel.immutable.ParVector

trait TransformationExtractor {
  def extract(tree: SyntaxTree): Set[Transformation] = {
    tree.traverseLeftRightBottomUp.iterator.
      flatMap(extractAtAnchorNode).
      toSet
  }

  protected def extractAtAnchorNode(node: Node): TraversableOnce[Transformation]
}

object TransformationExtractor extends TransformationExtractor with Logging {
  val extractors = List(Articulate, Flatten, Promote, Demote, Transfer, Adopt)

  protected def extractAtAnchorNode(node: Node): TraversableOnce[Transformation] =
    extractors.iterator.flatMap(_.extractAtAnchorNode(node))

  def findBestTransformation(alignTrees: ParVector[AlignmentTree]): Option[Transformation] = {
    val transToScoreDiff = alignTrees.
      flatMap(_.transformationToScoreDiff.iterator).
      groupBy(_._1).
      mapValues(_.map(_._2).sum)

    if (transToScoreDiff.isEmpty) None
    else {
      val (bestTrans, bestDiff) = transToScoreDiff.maxBy(_._2)
      if (bestDiff > 0) Some(bestTrans)
      else None
    }

  }

  def findBestTransformation(alignTrees: Vector[AlignmentTree]): Option[Transformation] =
    findBestTransformation(alignTrees.par)
}
