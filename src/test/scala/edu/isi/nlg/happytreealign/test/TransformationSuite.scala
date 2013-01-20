package edu.isi.nlg.happytreealign.test

import org.scalatest.FunSuite
import edu.isi.nlg.happytreealign._
import Direction._
import scala.Some

class TransformationSuite extends FunSuite {
  test("apply articulate transformation (example in original paper)") {
    val tree = SyntaxTree.parse("( (S (NP Other members) (VP will arrive in two groups) (. .)) )")
    val newTree = ArticulateTrans(parentLabel = "S", leftLabel = "NP", rightLabel = "VP")(tree).get

    assert(newTree.toString === "( (S (NP+VP (NP Other members) (VP will arrive in two groups)) (. .)) )")
  }

  test("apply articulate transformation (test for exhaustion)") {
    val tree = SyntaxTree.parse("( (a (b x) (c (a b c)) x b c) )")
    val newTree = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "c")(tree).get

    assert(newTree.toString === "( (a (b+c (b x) (c (a (b+c b c)))) x (b+c b c)) )")
  }

  test("apply articulate transformation (fail to apply)") {
    val articulate = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "c")
    val tree1 = SyntaxTree.parse("( (a (b x) (d (a b d)) x b d) )")
    val newTree1Op = articulate(tree1)
    assert(newTree1Op.isEmpty)

    val tree2 = articulate(SyntaxTree.parse("( (a (b x) (c (a b c)) x b c) )")).get
    val newTree2Op = ArticulateTrans(parentLabel = "a+b", leftLabel = "a", rightLabel = "b")(tree2)
    assert(newTree2Op.isEmpty)
  }

  test("extract articulate transformations") {
    val tree = SyntaxTree.parse("( (a (b x) (c (a b c)) x b c) )")
    assert(ArticulateTrans.extract(tree) === Set(
      ArticulateTrans("a", "b", "c"),
      ArticulateTrans("a", "c", "x"),
      ArticulateTrans("a", "x", "b")
    ))

    val newTree = ArticulateTrans(parentLabel = "a", leftLabel = "b", rightLabel = "c")(tree).get
    assert(ArticulateTrans.extract(newTree) === Set.empty)
  }

  test("apply flatten transformation (example in original paper)") {
    val tree = SyntaxTree.parse("( (NP (DT the) (NNP China) (NML (NNP Trade) (NNP Promotion)) (NNP Council)) )")
    val newTree: SyntaxTree = FlattenTrans("NP", "NML")(tree).get

    assert(newTree.toString === "( (NP (DT the) (NNP China) (NNP Trade) (NNP Promotion) (NNP Council)) )")
  }

  test("apply flatten transformation (test for exhaustion)") {
    val tree = SyntaxTree.parse("( (a (b (a (b a b)) b) (b a b)) )")
    val newTree: SyntaxTree = FlattenTrans("a", "b")(tree).get

    assert(newTree.toString === "( (a (a a b) b a b) )")
  }

  test("extract flatten transformations (both with and without context)") {
    val tree = SyntaxTree.parse("( (a l (b (d g h i) e) (c (f j k) m)) )")
    assert(FlattenTrans.extract(tree) === Set(
      FlattenTrans("a", "b"),
      FlattenTrans("a", "b", Some(("l", Right))),
      FlattenTrans("a", "b", Some(("c", Left))),
      FlattenTrans("c", "f"),
      FlattenTrans("c", "f", Some(("m", Left))),
      FlattenTrans("a", "c"),
      FlattenTrans("a", "c", Some(("b", Right)))
    ))
  }

  test("apply flatten transformation with context (example in original paper)") {
    val tree = SyntaxTree.parse("( (NP (DT the) (NNP1 China) (NML (NNP Trade) (NNP Promotion)) (NNP2 Council)) )")

    assert(
      FlattenTrans("NP", "NML", Some(("NNP1", Right)))(tree).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")
    assert(
      FlattenTrans("NP", "NML", Some(("NNP2", Left)))(tree).get.toString ===
        "( (NP (DT the) (NNP1 China) (NNP Trade) (NNP Promotion) (NNP2 Council)) )")

    assert(FlattenTrans("NP", "NML", Some(("NNP1", Left)))(tree).isEmpty)
    assert(FlattenTrans("NP", "NML", Some(("NNP2", Right)))(tree).isEmpty)
  }

  test("apply promote transformation") {
    val tree1 = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP1 N. Taugia))) )")
    assert(
      PromoteTrans("PP", "NP", "NP", Left)(tree1).get.toString ===
        "( (PP (IN by) (NP the French player) (NP (NP1 N. Taugia))) )")

    val tree2 = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP N. Taugia))) )")
    assert(
      PromoteTrans("NP", "NP", "player", Right)(tree2).get.toString ===
        "( (PP (IN by) (NP (NP the French) player (NP N. Taugia))) )")

    val tree3 = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP N. Taugia))) )")
    assert(
      PromoteTrans("PP", "NP", "NP", Left)(tree3).get.toString ===
        "( (PP (IN by) (NP the French player) (NP N. Taugia)) )")

    assert(PromoteTrans("PP", "NP", "NP", Right)(tree3).isEmpty)
  }

  test("extract promote transformations") {
    val tree = SyntaxTree.parse("( (PP (IN by) (NP (NP the French player) (NP N. Taugia))) )")
    assert(PromoteTrans.extract(tree) === Set(
      PromoteTrans("PP", "IN", "by", Right),
      PromoteTrans("PP", "NP", "NP", Left),
      PromoteTrans("NP", "NP", "player", Right),
      PromoteTrans("NP", "NP", "N.", Left)
    ))
  }

  test("apply demote transformation") {
    val tree = SyntaxTree.parse("( (VP (VB fly) (PP (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(DemoteTrans("VP", "PP", "VB", Right)(tree).get.toString ===
      "( (VP (PP (VB fly) (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(DemoteTrans("VP", "PP", "PP", Left)(tree).get.toString ===
      "( (VP (VB fly) (PP (IN to) (NP Beijing) (PP (IN on) (NP the 2nd)))) )")
    assert(DemoteTrans("PP", "IN", "NP", Left)(tree).get.toString ===
      "( (VP (VB fly) (PP (IN to (NP Beijing))) (PP (IN on (NP the 2nd)))) )")
    assert(DemoteTrans("PP", "NP", "IN", Right)(tree).get.toString ===
      "( (VP (VB fly) (PP (NP (IN to) Beijing)) (PP (NP (IN on) the 2nd))) )")
    assert(DemoteTrans("NP", "the", "2nd", Left)(tree).isEmpty)
  }

  test("extract demote transformations") {
    val tree = SyntaxTree.parse("( (VP (VB fly) (PP (IN to) (NP Beijing)) (PP (IN on) (NP the 2nd))) )")
    assert(DemoteTrans.extract(tree) === Set(
      DemoteTrans("VP", "VB", "PP", Left),
      DemoteTrans("VP", "PP", "VB", Right),
      DemoteTrans("VP", "PP", "PP", Left),
      DemoteTrans("VP", "PP", "PP", Right),
      DemoteTrans("PP", "IN", "NP", Left),
      DemoteTrans("PP", "NP", "IN", Right)
    ))
  }
}
