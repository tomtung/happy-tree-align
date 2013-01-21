package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import Direction._

case class PromoteTrans(grandparentLabel: String,
                        parentLabel: String,
                        childLabel: String,
                        direction: Direction) extends Transformation {
  override protected def applyOnAnchorNode(grandparent: Node): Option[Node] = {
    if (grandparent.isLeaf || grandparent.label != grandparentLabel)
      None
    else {
      for (
        i <- (0 until grandparent.children.length).iterator;

        parent = grandparent.children(i)
        if !parent.isLeaf &&
          parent.label == parentLabel &&
          !parent.isPos;

        child = direction match {
          case Left => parent.children.head
          case Right => parent.children.last
        }
        if child.label == childLabel;

        patch = direction match {
          case _ if parent.children.length == 1 =>
            List(child)
          case Left =>
            List(child, new Node(parent.label, parent.children.drop(1)))
          case Right =>
            List(new Node(parent.label, parent.children.dropRight(1)), child)
        };

        updatedGrandparent = {
          val updatedChildren = grandparent.children.patch(i, patch, 1)
          new Node(grandparent.label, updatedChildren)
        }
      ) yield updatedGrandparent
    }.toIterable.headOption
  }
}

object PromoteTrans extends TransformationExtractor {
  override protected def extractAtAnchorNode(grandparent: Node): TraversableOnce[Transformation] = {
    for (
      i <- (0 until grandparent.children.length).iterator;

      parent = grandparent.children(i)
      if !parent.isLeaf && !parent.isPos;

      leftTrans = PromoteTrans(grandparent.label, parent.label, parent.children.head.label, Left);
      rightTrans = PromoteTrans(grandparent.label, parent.label, parent.children.last.label, Right);

      trans <- List(leftTrans, rightTrans)
    ) yield trans
  }
}
