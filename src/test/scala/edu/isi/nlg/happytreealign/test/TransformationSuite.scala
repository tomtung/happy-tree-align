package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign._
import Direction._
import scala.Some

class TransformationSuite extends FunSuite {
  test("apply articulate transformation") {
    val tree1 = SyntaxTree.parse("( (S (NP (DUMMY Other) members) (VP will (DUMMY arrive) in two groups) (. .)) )")
    assert(ArticulateTrans(parentLabel = "S", leftLabel = "NP", rightLabel = "VP")(tree1).get.toString ===
      "( (S (NP+VP (NP (DUMMY Other) members) (VP will (DUMMY arrive) in two groups)) (. .)) )")

    val tree2 = SyntaxTree.parse("( (a (a (b x) (c x) x) b c) )")
    val newTree2 = ArticulateTrans("a", "b", "c")(tree2).get
    assert(newTree2.toString ===
      "( (a (a (b+c (b x) (c x)) x) (b+c b c)) )")

    assert(ArticulateTrans("a+b", "a", "b")(newTree2).isEmpty)

    val tree3 = SyntaxTree.parse("( (a b c) )")
    assert(ArticulateTrans("a", "b", "c")(tree3).isEmpty)
  }

  test("extract articulate transformations") {
    val tree = SyntaxTree.parse("( (a (b x x x) (a b (a x)) c) )")
    assert(ArticulateTrans.extract(tree) === Set(
      ArticulateTrans("a", "b", "a"),
      ArticulateTrans("a", "a", "c")
    ))

    val newTree = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "a")(tree).get
    assert(ArticulateTrans.extract(newTree) === Set.empty)
  }

  test("apply flatten transformation") {
    val tree1 = SyntaxTree.parse("( (NP (DT the) (NNP1 China) (NML (NNP Trade) (NNP Promotion)) (NNP2 Council)) )")
    assert(FlattenTrans("NP", "NML")(tree1).get.toString ===
      "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(
      FlattenTrans("NP", "NML", Some(("NNP1", Right)))(tree1).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")
    assert(
      FlattenTrans("NP", "NML", Some(("NNP2", Left)))(tree1).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(FlattenTrans("NP", "NML", Some(("NNP1", Left)))(tree1).isEmpty)
    assert(FlattenTrans("NP", "NML", Some(("NNP2", Right)))(tree1).isEmpty)

    val tree2 = SyntaxTree.parse("( (a (b x (b x (b x x))) c) )")
    assert(FlattenTrans("a", "b", Some("c" -> Left))(tree2).get.toString ===
      "( (a x x (b x x) c) )")
  }

  test("extract flatten transformations") {
    val tree = SyntaxTree.parse("( (a (b d (e (k m) l)) (c f g) (d h i j)) )")
    assert(FlattenTrans.extract(tree) === Set(
      FlattenTrans("a", "b"),
      FlattenTrans("a", "b", Some("c" -> Left)),
      FlattenTrans("b", "e"),
      FlattenTrans("b", "e", Some("d" -> Right))
    ))
  }

  test("apply promote transformation") {
    val tree = SyntaxTree.parse("( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (PP (IN of) (NP the UN))) )")
    assert(PromoteTrans("NP", "NP", "DT", Left)(tree).get.toString ===
      "( (NP (DT all) (NP (JJ peacekeeping) (NNS forces)) (PP (IN of) (NP the UN))) )")
    assert(PromoteTrans("NP", "NP", "NNS", Right)(tree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping)) (NNS forces) (PP (IN of) (NP the UN))) )")
    val newTree = PromoteTrans("NP", "PP", "IN", Left)(tree).get
    assert(newTree.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (IN of) (PP (NP the UN))) )")
    assert(PromoteTrans("NP", "PP", "NP", Right)(newTree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (IN of) (NP the UN)) )")
    assert(PromoteTrans("NP", "PP", "NP", Right)(tree).get.toString ===
      "( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (PP (IN of)) (NP the UN)) )")
    assert(PromoteTrans("NP", "JJ", "peacekeeping", Left)(tree).isEmpty)
    assert(PromoteTrans("NP", "JJ", "peacekeeping", Right)(tree).isEmpty)
  }

  test("extract promote transformations") {
    val tree = SyntaxTree.parse("( (NP (NP (DT all) (JJ peacekeeping) (NNS forces)) (PP (IN of) (NP the UN))) )")
    assert(PromoteTrans.extract(tree) === Set(
      PromoteTrans("NP", "NP", "DT", Left),
      PromoteTrans("NP", "NP", "NNS", Right),
      PromoteTrans("NP", "PP", "IN", Left),
      PromoteTrans("NP", "PP", "NP", Right)
    ))
  }

  test("apply demote transformation") {
    val tree = SyntaxTree.parse("( (VP (VB fly) (PP (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(DemoteTrans("VP", "PP", "VB", Right)(tree).get.toString ===
      "( (VP (PP (VB fly) (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(DemoteTrans("VP", "PP", "PP", Left)(tree).get.toString ===
      "( (VP (VB fly) (PP (IN to) (NP Beijing) (PP (IN on) (NP the 2nd)))) )")
    assert(DemoteTrans("PP", "IN", "NP", Left)(tree).isEmpty)
    assert(DemoteTrans("PP", "NP", "IN", Right)(tree).isEmpty)
    assert(DemoteTrans("NP", "the", "2nd", Left)(tree).isEmpty)
  }

  test("extract demote transformations") {
    val tree = SyntaxTree.parse("( (VP (VB fly) (PP (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(DemoteTrans.extract(tree) === Set(
      DemoteTrans("VP", "PP", "PP", Left),
      DemoteTrans("VP", "PP", "PP", Right),
      DemoteTrans("VP", "PP", "VB", Right)
    ))
  }

  test("apply transfer transformation") {
    val tree = SyntaxTree.parse("( (NP (NP (JJ serious) (NNS consequences)) (SBAR (WHNP that) (S cause losses))) )")
    assert(TransferTrans("NP", "NP", "SBAR", "WHNP", Left)(tree).get.toString ===
      "( (NP (NP (JJ serious) (NNS consequences) (WHNP that)) (SBAR (S cause losses))) )")
    assert(TransferTrans("NP", "SBAR", "NP", "NNS", Right)(tree).get.toString ===
      "( (NP (NP (JJ serious)) (SBAR (NNS consequences) (WHNP that) (S cause losses))) )")
    assert(TransferTrans("NP", "JJ", "NNS", "consequences", Left)(tree).isEmpty)
    assert(TransferTrans("NP", "NNS", "JJ", "serious", Right)(tree).isEmpty)
  }

  test("extract transfer transformations") {
    val tree = SyntaxTree.parse("( (NP (NP (JJ serious) (NNS consequences)) (SBAR (WHNP that) (S cause losses))) )")
    assert(TransferTrans.extract(tree) === Set(
      TransferTrans("NP", "NP", "SBAR", "WHNP", Left),
      TransferTrans("NP", "SBAR", "NP", "NNS", Right)
    ))
  }
}
