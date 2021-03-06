package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import annotation.tailrec

trait Transformation {
  protected def applyOnAnchorNode(node: Node): Option[Node]

  def applyOption(tree: SyntaxTree): Option[SyntaxTree] = {
    @tailrec
    def doApply(tree: SyntaxTree, returnNoneIfFail: Boolean): Option[SyntaxTree] = {
      val pairOp =
        tree.traverseLeftRightBottomUp.iterator.
          flatMap(node => applyOnAnchorNode(node).map(node -> _)).
          toIterable.headOption

      pairOp match {
        case None if returnNoneIfFail => None
        case None => Some(tree)
        case Some((pNode, updatedPNode)) =>
          doApply(tree.replace(pNode, updatedPNode), returnNoneIfFail = false)
      }
    }

    doApply(tree, returnNoneIfFail = true)
  }

  def apply(tree: SyntaxTree): SyntaxTree = {
    applyOption(tree).getOrElse(tree)
  }

  def applyOption(alignmentTree: AlignmentTree): Option[AlignmentTree] = {
    applyOption(alignmentTree.syntaxTree).map(
      t => new AlignmentTree(t, alignmentTree.alignment))
  }

  def apply(alignmentTree: AlignmentTree): AlignmentTree = {
    applyOption(alignmentTree).getOrElse(alignmentTree)
  }
}
