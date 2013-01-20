package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import Direction._

case class FlattenTrans(parentLabel: String,
                        targetLabel: String,
                        siblingLabelAndDirection: Option[(String, Direction)] = None) extends Transformation {
  override protected def applyOnAnchorNode(parent: Node): Option[Node] = {
    if (parent.isLeaf || parent.label != parentLabel)
      return None

    val children = parent.children
    val idxOp = (0 until children.length).find(
      i => {
        val t = children(i)
        t.label == targetLabel && t.children.length == 2 &&
          ((siblingLabelAndDirection: @unchecked) match {
            case None => true
            case Some((siblingLabel, Right)) =>
              i > 0 && children(i - 1).label == siblingLabel
            case Some((siblingLabel, Left)) =>
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
    def extractAtTargetPos(i: Int, parent: Node): List[FlattenTrans] = {
      val target = parent.children(i)
      val trans = FlattenTrans(parent.label, target.label)

      trans :: List(i - 1 -> Right, i + 1 -> Left).filter({
        case (j, _) =>
          0 <= j && j < parent.children.length
      }).map({
        case (j, d) =>
          trans.copy(siblingLabelAndDirection = Some(
            parent.children(j).label -> d
          ))
      })
    }

    for (
      p <- tree.traverseLeftRightBottomUp.iterator
      if !p.isLeaf;
      (t, tPos) <- p.children.iterator.zipWithIndex
      if (t.children.length == 2);
      tr <- extractAtTargetPos(tPos, p)
    ) yield tr
  }.toSet
}
