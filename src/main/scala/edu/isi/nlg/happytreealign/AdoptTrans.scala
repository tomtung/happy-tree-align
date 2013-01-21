package edu.isi.nlg.happytreealign

import Direction._
import edu.isi.nlg.happytreealign.SyntaxTree.Node

case class AdoptTrans(grandparentLabel: String,
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
          aunt.label == auntLabel &&
          parent.label == parentLabel &&
          !parent.isPos;

        target = direction match {
          case Left => parent.children.head
          case Right => parent.children.last
        }
        if target.label == targetLabel;

        mergedNode = direction match {
          case Left => new Node(aunt.label + "+" + target.label, Vector(aunt, target), isMerged = true)
          case Right => new Node(target.label + "+" + aunt.label, Vector(target, aunt), isMerged = true)
        };

        patch = {
          direction match {
            case _ if parent.children.length == 1 =>
              List(mergedNode)
            case Left =>
              val updatedParent = new Node(parent.label, parent.children.drop(1))
              List(mergedNode, updatedParent)
            case Right =>
              val updatedParent = new Node(parent.label, parent.children.dropRight(1))
              List(updatedParent, mergedNode)
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

object AdoptTrans extends TransformationExtractor {
  protected def extractAtAnchorNode(grandparent: Node): TraversableOnce[Transformation] = {
    var lst: List[AdoptTrans] = Nil
    for (
      i <- 0 until grandparent.children.length - 1;
      l = grandparent.children(i);
      r = grandparent.children(i + 1)
    ) {
      if (!l.isLeaf && !l.isPos) {
        lst ::= AdoptTrans(grandparent.label, r.label, l.label, l.children.last.label, Right)
      }
      if (!r.isLeaf && !r.isPos) {
        lst ::= AdoptTrans(grandparent.label, l.label, r.label, r.children.head.label, Left)
      }
    }

    lst
  }
}
