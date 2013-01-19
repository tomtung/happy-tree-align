package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import annotation.tailrec

case class FlattenTrans(parentLabel: String, targetLabel: String) {
  def apply(tree: SyntaxTree): Option[SyntaxTree] = {
    def applyOnParentNode(parent: Node): Option[Node] = {
      if (parent.isLeaf || parent.label != parentLabel)
        return None

      val children = parent.children
      val idx = children.indexWhere(t => t.label == targetLabel && t.children.length == 2)

      if (idx < 0) None
      else {
        val updatedChildren = children.patch(idx, children(idx).children, 1)
        val updateParent = new Node(parent.label, updatedChildren)
        Some(updateParent)
      }
    }

    // TODO same code as in articulate trans. template pattern might solve this, let's see.
    @tailrec
    def doApply(tree: SyntaxTree, returnNoneIfFail: Boolean): Option[SyntaxTree] = {
      val pairOp = tree.traverseLeftRightBottomUp.iterator.
        map(node => node -> applyOnParentNode(node)).
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

object FlattenTrans {
  def extractFrom(tree: SyntaxTree): Set[FlattenTrans] = {
    tree.traverseLeftRightBottomUp.iterator.
      filterNot(p => p.isLeaf).
      flatMap(
      p => {
        p.children.iterator.
          filter(_.children.length == 2).
          map(t => FlattenTrans(p.label, t.label))
      }).
      toSet
  }
}