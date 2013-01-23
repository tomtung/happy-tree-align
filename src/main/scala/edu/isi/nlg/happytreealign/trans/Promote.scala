package edu.isi.nlg.happytreealign.trans

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import edu.isi.nlg.happytreealign._

case class Promote(grandparentLabel: String,
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
            val updatedParent = parent.childrenUpdated(parent.children.drop(1))
            List(child, updatedParent)
          case Right =>
            val updatedParent = parent.childrenUpdated(parent.children.dropRight(1))
            List(updatedParent, child)
        };

        updatedGrandparent = {
          val updatedChildren = grandparent.children.patch(i, patch, 1)
          grandparent.childrenUpdated(updatedChildren)
        }
      ) yield updatedGrandparent
    }.toIterable.headOption
  }
}

object Promote extends TransformationExtractor {
  override protected def extractAtAnchorNode(grandparent: Node): TraversableOnce[Transformation] = {
    for (
      i <- (0 until grandparent.children.length).iterator;

      parent = grandparent.children(i)
      if !parent.isLeaf && !parent.isPos;

      leftTrans = Promote(grandparent.label, parent.label, parent.children.head.label, Left);
      rightTrans = Promote(grandparent.label, parent.label, parent.children.last.label, Right);

      trans <- List(leftTrans, rightTrans)
    ) yield trans
  }
}
