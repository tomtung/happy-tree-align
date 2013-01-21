package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import Direction._

case class FlattenTrans(parentLabel: String,
                        targetLabel: String,
                        siblingLabelAndDirection: Option[(String, Direction)] = None) extends Transformation {
  override protected def applyOnAnchorNode(parent: Node): Option[Node] = {
    if (parent.isLeaf || parent.label != parentLabel) None
    else {
      val children = parent.children
      for (
        i <- (0 until children.length).iterator;

        target = children(i)
        if target.label == targetLabel && target.children.length == 2 &&
          ((siblingLabelAndDirection: @unchecked) match {
            case None => true
            case Some((siblingLabel, Right)) =>
              i > 0 && children(i - 1).label == siblingLabel
            case Some((siblingLabel, Left)) =>
              i < children.length - 1 && children(i + 1).label == siblingLabel
          });

        updatedChildren = children.patch(i, target.children, 1);
        updatedParent = new Node(parent.label, updatedChildren)
      ) yield updatedParent
    }.toIterable.headOption
  }
}

object FlattenTrans extends TransformationExtractor {
  override protected def extractAtAnchorNode(parent: Node): TraversableOnce[Transformation] = {
    def extractAtTargetPos(i: Int): List[FlattenTrans] = {
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
      (target, tPos) <- parent.children.iterator.zipWithIndex
      if (target.children.length == 2);
      trans <- extractAtTargetPos(tPos)
    ) yield trans
  }
}