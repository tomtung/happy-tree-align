package edu.isi.nlg.happytreealign

import util.parsing.combinator.RegexParsers

case class ParseTree(label: String, children: IndexedSeq[ParseTree] = IndexedSeq.empty[ParseTree])

object ParseTree {

  object Parser extends RegexParsers {

    def identifier: Parser[String] = regex( """[^\(\)\s]+""".r)

    def atom: Parser[ParseTree] = identifier ^^ {
      s => ParseTree(s)
    }

    def list: Parser[ParseTree] = "(" ~> identifier ~ rep(sExpr) <~ ")" ^^ {
      case root ~ ch => ParseTree(root, ch.toIndexedSeq)
    }

    def sExpr: Parser[ParseTree] = atom | list

    def start: Parser[ParseTree] = sExpr | "(" ~> sExpr <~ ")"

    def apply(s: String): ParseTree = {
      parse(start, s) match {
        case Success(result, _) => result
        case NoSuccess(msg, _) =>
          throw new ParsingException("Invalid tree string: " + s + "; " + msg)
      }
    }
  }

  def parse(s: String): ParseTree = {
    Parser(s)
  }
}
