package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import Direction._

case class TransferTrans(grandparentLabel: String,
                         auntLabel: String,
                         parentLabel: String,
                         targetLabel: String,
                         direction: Direction) extends Transformation {
  protected override def applyOnAnchorNode(grandparent: Node): Option[Node] = {
    if (grandparent.isLeaf ||
      grandparent.children.length != 2 ||
      grandparent.label != grandparentLabel ||
      grandparent.children.exists(_.isLeaf))
      return None

    val (parent, target, aunt) = direction match {
      case Left =>
        (grandparent.children(1), grandparent.children(1).children.head, grandparent.children(0))
      case Right =>
        (grandparent.children(0), grandparent.children(0).children.last, grandparent.children(1))
    }

    if (parent.label != parentLabel ||
      aunt.label != auntLabel ||
      target.label != targetLabel)
      return None

    val updatedChildren = direction match {
      case Left =>
        val updatedAunt = new Node(aunt.label, aunt.children :+ target)
        if (parent.children.length == 1) Vector(updatedAunt)
        else {
          val updatedParent = new Node(parent.label, parent.children.drop(1))
          Vector(updatedAunt, updatedParent)
        }
      case Right =>
        val updatedAunt = new Node(aunt.label, target +: aunt.children)
        if (parent.children.length == 1) Vector(updatedAunt)
        else {
          val updatedParent = new Node(parent.label, parent.children.dropRight(1))
          Vector(updatedParent, updatedAunt)
        }
    }

    Some(new Node(grandparent.label, updatedChildren))
  }
}

object TransferTrans extends TransformationExtractor {
  protected override def extractAtAnchorNode(grandparent: Node): TraversableOnce[Transformation] = {
    var lst: List[Transformation] = Nil

    if (grandparent.children.length == 2 &&
      !grandparent.children.exists(_.isLeaf)) {
      val (l, r) = (grandparent.children(0), grandparent.children(1))
      lst =
        TransferTrans(grandparent.label, l.label, r.label, r.children.head.label, Left) ::
        TransferTrans(grandparent.label, r.label, l.label, l.children.last.label, Right) :: lst
    }

    lst
  }
}
