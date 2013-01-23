package edu.isi.nlg.happytreealign.trans

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import edu.isi.nlg.happytreealign._

case class Flatten(parentLabel: String,
                   targetLabel: String,
                   siblingLabelAndDirection: Option[(String, Direction)] = None) extends Transformation {
  override protected def applyOnAnchorNode(parent: Node): Option[Node] = {
    if (parent.isLeaf || parent.label != parentLabel) None
    else {
      val children = parent.children
      for (
        i <- (0 until children.length).iterator;

        target = children(i)
        if target.children.length == 2 &&
          !target.isPos &&
          target.label == targetLabel &&
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

object Flatten extends TransformationExtractor {
  override protected def extractAtAnchorNode(parent: Node): TraversableOnce[Transformation] = {
    def extractAtTargetPos(i: Int): List[Flatten] = {
      val target = parent.children(i)
      val trans = Flatten(parent.label, target.label)

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
      if target.children.length == 2 && !target.isPos;
      trans <- extractAtTargetPos(tPos)
    ) yield trans
  }
}
