package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import Direction._

case class FlattenTrans(parentLabel: String,
                        targetLabel: String,
                        siblingLabelAndDirection: Option[(String, Direction)] = None) extends Transformation {
  override protected def applyOnNodeAsParent(parent: Node): Option[Node] = {
    if (parent.isLeaf || parent.label != parentLabel)
      return None

    val children = parent.children
    val idxOp = (0 until children.length).find(
      i => {
        val t = children(i)
        t.label == targetLabel && t.children.length == 2 &&
          ((siblingLabelAndDirection: @unchecked) match {
            case None => true
            case Some((siblingLabel, Left)) =>
              i > 0 && children(i - 1).label == siblingLabel
            case Some((siblingLabel, Right)) =>
              i < children.length - 1 && children(i + 1).label == siblingLabel
          })
      })

    idxOp.map(idx => {
      val updatedChildren = children.patch(idx, children(idx).children, 1)
      val updatedParent = new Node(parent.label, updatedChildren)
      updatedParent
    })
  }
}

object FlattenTrans extends TransformationExtractor {
  override def extract(tree: SyntaxTree) = {
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