package edu.isi.nlg.happytreealign.trans

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import edu.isi.nlg.happytreealign.Direction._
import edu.isi.nlg.happytreealign.{TransformationExtractor, Transformation}

case class Transfer(grandparentLabel: String,
                         auntLabel: String,
                         parentLabel: String,
                         targetLabel: String,
                         direction: Direction) extends Transformation {
  protected override def applyOnAnchorNode(grandparent: Node): Option[Node] = {
    if (grandparent.children.length < 2 || grandparent.label != grandparentLabel) None
    else {
      for (
        i <- (0 until grandparent.children.length - 1).iterator;

        (aunt, parent) = direction match {
          case Left => grandparent.children(i) -> grandparent.children(i + 1)
          case Right => grandparent.children(i + 1) -> grandparent.children(i)
        }
        if !aunt.isLeaf && aunt.label == auntLabel &&
          !parent.isLeaf && parent.label == parentLabel &&
          !aunt.isPos && !parent.isPos;

        target = direction match {
          case Left => parent.children.head
          case Right => parent.children.last
        }
        if target.label == targetLabel;

        updatedAunt = {
          val updatedChildren = direction match {
            case Left => aunt.children :+ target
            case Right => target +: aunt.children
          }

          new Node(aunt.label, updatedChildren)
        };

        patch = {
          direction match {
            case _ if parent.children.length == 1 =>
              List(updatedAunt)
            case Left =>
              val updatedParent = new Node(parent.label, parent.children.drop(1))
              List(updatedAunt, updatedParent)
            case Right =>
              val updatedParent = new Node(parent.label, parent.children.dropRight(1))
              List(updatedParent, updatedAunt)
          }
        };

        updatedGrandparent = {
          val updatedChildren = grandparent.children.patch(i, patch, 2)
          new Node(grandparent.label, updatedChildren)
        }
      ) yield updatedGrandparent
    }.toIterable.headOption
  }
}

object Transfer extends TransformationExtractor {
  protected override def extractAtAnchorNode(grandparent: Node): TraversableOnce[Transformation] = {
    for (
      i <- (0 until grandparent.children.length - 1).iterator;
      (l, r) = (grandparent.children(i), grandparent.children(i + 1))
      if !l.isLeaf && !l.isPos && !r.isLeaf && !r.isPos;

      lTrans = Transfer(grandparent.label, l.label, r.label, r.children.head.label, Left);
      rTrans = Transfer(grandparent.label, r.label, l.label, l.children.last.label, Right);

      trans <- List(lTrans, rTrans)
    ) yield trans
  }
}
