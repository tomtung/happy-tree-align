package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node

trait TransformationExtractor {
  def extract(tree: SyntaxTree): Set[Transformation] = {
    tree.traverseLeftRightBottomUp.iterator.
      flatMap(extractAtAnchorNode).
      toSet
  }

  protected def extractAtAnchorNode(node: Node): TraversableOnce[Transformation]
}
