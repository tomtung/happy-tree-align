package edu.isi.nlg.happytreealign.trans

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import edu.isi.nlg.happytreealign._
import edu.isi.nlg.happytreealign.Direction._

case class Adopt(grandparentLabel: String,
                 auntLabel: String,
                 parentLabel: String,
                 targetLabel: String,
                 direction: Direction) extends Transformation {
  protected def applyOnAnchorNode(grandparent: Node): Option[Node] = {
    if (grandparent.children.length < 2 || grandparent.label != grandparentLabel) None
    else {
      for (
        i <- (0 until grandparent.children.length - 1).iterator;

        (aunt, parent) = direction match {
          case Left => grandparent.children(i) -> grandparent.children(i + 1)
          case Right => grandparent.children(i + 1) -> grandparent.children(i)
        }
        if !parent.isLeaf &&
          !aunt.isMerged &&
          aunt.label == auntLabel &&
          parent.label == parentLabel &&
          !parent.isPos;

        target = direction match {
          case Left => parent.children.head
          case Right => parent.children.last
        }
        if !target.isMerged && target.label == targetLabel;

        mergedNode = direction match {
          case Left => new Node(aunt.label + "+" + target.label, Vector(aunt, target), isMerged = true)
          case Right => new Node(target.label + "+" + aunt.label, Vector(target, aunt), isMerged = true)
        };

        patch = {
          direction match {
            case _ if parent.children.length == 1 =>
              List(mergedNode)
            case Left =>
              val updatedParent = parent.childrenUpdated(parent.children.drop(1))
              List(mergedNode, updatedParent)
            case Right =>
              val updatedParent = parent.childrenUpdated(parent.children.dropRight(1))
              List(updatedParent, mergedNode)
          }
        };

        updatedGrandparent = {
          val updatedChildren = grandparent.children.patch(i, patch, 2)
          grandparent.childrenUpdated(updatedChildren)
        }
      ) yield updatedGrandparent
    }.toIterable.headOption
  }
}

object Adopt extends TransformationExtractor {
  protected def extractAtAnchorNode(grandparent: Node): TraversableOnce[Transformation] = {
    var lst: List[Adopt] = Nil
    for (
      i <- 0 until grandparent.children.length - 1;
      l = grandparent.children(i);
      r = grandparent.children(i + 1)
    ) {
      if (!r.isMerged && !l.isLeaf && !l.children.last.isMerged && !l.isPos) {
        lst ::= Adopt(grandparent.label,
          auntLabel = r.label,
          parentLabel = l.label,
          targetLabel = l.children.last.label,
          direction = Right)
      }
      if (!l.isMerged && !r.isLeaf && !r.children.head.isMerged && !r.isPos) {
        lst ::= Adopt(grandparent.label,
          auntLabel = l.label,
          parentLabel = r.label,
          targetLabel = r.children.head.label,
          direction = Left)
      }
    }

    lst
  }
}
