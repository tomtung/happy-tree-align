package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import trans._
import com.typesafe.scalalogging.slf4j.Logging
import collection.mutable

trait TransformationExtractor {
  def extract(tree: SyntaxTree): Set[Transformation] = {
    tree.traverseLeftRightBottomUp.iterator.
      flatMap(extractAtAnchorNode).
      toSet
  }

  def extract(alignTree: AlignmentTree): Set[Transformation] =
    extract(alignTree.syntaxTree)

  protected def extractAtAnchorNode(node: Node): TraversableOnce[Transformation]
}

object TransformationExtractor extends TransformationExtractor with Logging {
  val extractors = List(Articulate, Flatten, Promote, Demote, Transfer, Adopt)

  protected def extractAtAnchorNode(node: Node): TraversableOnce[Transformation] =
    extractors.iterator.flatMap(_.extractAtAnchorNode(node))

  // TODO how to parallelize this?
  def findBestTransformation(alignTrees: Vector[AlignmentTree]): Option[Transformation] = {
    val transToScoreDiff = mutable.Map[Transformation, Int]().withDefaultValue(0)
    transToScoreDiff.sizeHint(200 * alignTrees.size)

    for (
      alignTree <- alignTrees;
      trans <- extract(alignTree).iterator;
      newAlignTree = trans(alignTree);
      scoreDiff = newAlignTree.agreementScore - alignTree.agreementScore
      if scoreDiff != 0
    ) {
      transToScoreDiff(trans) += scoreDiff
    }

    if (transToScoreDiff.isEmpty) None
    else {
      val (bestTrans, bestDiff) = transToScoreDiff.maxBy(_._2)
      if (bestDiff > 0) Some(bestTrans)
      else None
    }
  }
}
