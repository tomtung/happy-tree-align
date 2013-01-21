package edu.isi.nlg.happytreealign

import edu.isi.nlg.happytreealign.SyntaxTree.Node
import Direction._

case class DemoteTrans(parentLabel: String,
                       demoterLabel: String,
                       demotedLabel: String,
                       direction: Direction) extends Transformation {
  override protected def applyOnAnchorNode(parent: Node): Option[Node] = {
    if (parent.children.length < 2 || parent.label != parentLabel) None
    else {
      val children = parent.children
      for (
        i <- (0 until children.length - 1).iterator;

        (demoter, demoted) = direction match {
          case Left => (children(i), children(i + 1))
          case Right => (children(i + 1), children(i))
        }
        if !demoter.isLeaf &&
          demoter.label == demoterLabel &&
          demoted.label == demotedLabel;

        updatedDemoter = {
          val updatedDemoterChildren = direction match {
            case Left => demoter.children :+ demoted
            case Right => demoted +: demoter.children
          }
          new Node(demoter.label, updatedDemoterChildren)
        };

        updatedParent = new Node(parent.label, parent.children.patch(i, List(updatedDemoter), 2))
      ) yield updatedParent
    }.toIterable.headOption
  }
}

object DemoteTrans extends TransformationExtractor {
  protected def extractAtAnchorNode(parent: Node): TraversableOnce[Transformation] = {
    var lst: List[DemoteTrans] = Nil
    for (
      i <- 0 until parent.children.length - 1;
      l = parent.children(i);
      r = parent.children(i + 1)
    ) {
      if (!l.isLeaf) {
        lst ::= DemoteTrans(parent.label, l.label, r.label, Left)
      }
      if (!r.isLeaf) {
        lst ::= DemoteTrans(parent.label, r.label, l.label, Right)
      }
    }

    lst
  }
}