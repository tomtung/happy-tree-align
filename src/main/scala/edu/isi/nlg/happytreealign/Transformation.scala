package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import annotation.tailrec

abstract class Transformation {
  protected def applyOnNodeAsParent(parent: Node): Option[Node]

  def apply(tree: SyntaxTree): Option[SyntaxTree] = {
    @tailrec
    def doApply(tree: SyntaxTree, returnNoneIfFail: Boolean): Option[SyntaxTree] = {
      val pairOp = tree.traverseLeftRightBottomUp.iterator.
        map(node => node -> applyOnNodeAsParent(node)).
        find(_._2.isDefined)
      pairOp match {
        case None if returnNoneIfFail => None
        case None => Some(tree)
        case Some((parent, Some(updatedParent))) =>
          doApply(tree.replace(parent, updatedParent), returnNoneIfFail = false)
      }
    }

    doApply(tree, returnNoneIfFail = true)
  }
}
