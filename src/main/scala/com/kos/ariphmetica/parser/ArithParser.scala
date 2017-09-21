package com.kos.ariphmetica.parser

import com.kos.ariphmetica.math.functions.Fun
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math._
import com.sun.org.apache.xpath.internal.functions.FunctionMultiArgs
import org.parboiled2._

/**
  * Created by Kos on 19.03.2017.
  */


abstract class ArithParser(val input: ParserInput
				  ) extends Parser with WordParser {

	def divMethod: (MathTerm,MathTerm) ⇒ MathTerm
	def powMethod: (MathTerm,MathTerm) ⇒ MathTerm
	def functionMethod: (String,String, collection.immutable.Seq[MathTerm]) ⇒ MathTerm = Fun.apply
	def functionAbsMethod: (String,String ,collection.immutable.Seq[MathTerm]) ⇒ MathTerm = Fun.absApply


	def InputLine = rule {
		top ~ EOI
	}


	def top: Rule1[MathTerm] = rule {
		ravno
	}

	def ravno: Rule1[MathTerm] = rule {
		comp ~ zeroOrMore(
			"=" ~ comp ~> (MathTerm3(_: MathTerm, Operator.equa, _))
		)
	}

	def comp: Rule1[MathTerm] = rule {
		expr ~ zeroOrMore(
			  "<" ~ expr ~> (MathTerm3(_: MathTerm, Operator.less, _))
			| ">" ~ expr ~> (MathTerm3(_: MathTerm, Operator.more, _))
		) //~> operators
	}



	def expr: Rule1[MathTerm] = rule {
		termFirst ~ zeroOrMore(
			"+" ~ term ~> (MathTerm3(_: MathTerm, Operator.add, _))
			| minus ~ term ~> (MathTerm3(_: MathTerm, Operator.sub, _))
			//			| "−" ~ term ~> (MathTerm3(_: MathTerm, Operator.sub, _))
		) // ~> operators
	}

	def termFirst: Rule1[MathTerm] = rule {
		(minus ~ term ~> {MathTerm2(Operator.neg, _)}
		| term
		)
	}

	def term: Rule1[MathTerm] = rule {
		factor ~ zeroOrMore(
			  "*" ~ factor ~> (MathTerm3(_: MathTerm, Operator.mul, _))
			| "/" ~ factor ~> (divMethod(_:MathTerm,_) )
			| "%" ~ factor ~> (MathTerm3(_: MathTerm, Operator.mod, _))
			| "×" ~ factor ~> (MathTerm3(_: MathTerm, Operator.mul, _))
			| "÷" ~ factor ~> (divMethod(_:MathTerm,_) )
			| "∙" ~ factor ~> (MathTerm3(_: MathTerm, Operator.mul, _))
		) //~> operators
	}


	def factor: Rule1[MathTerm] = rule {
		values ~ zeroOrMore(
			"^" ~ values ~> powMethod
		)
	}


	def values: Rule1[MathTerm] = rule {
			( floatingPointNumber ~> { a => tryToNum(a) }
	//		| minus ~ minusValues ~> { MathTerm2(Operator.neg,_) }
			| minusValues
			)
	}

	def minusValues:  Rule1[MathTerm] = rule {
		( word ~ capture(zeroOrMore("'")) ~ "(" ~ funArguments ~ ")" ~> { functionMethod }
		| word ~ capture(zeroOrMore("'")) ~ "|" ~ funArguments ~ "|" ~> { functionAbsMethod }
		| word ~> MathConst
		| "(" ~ top ~ ")" ~ zeroOrMore("'" ~ word ~> (DiffTerm(_: MathTerm, _)))
		| "|" ~ top ~ "|" ~> (a ⇒ MathTerm2(Operator.abs, a))
		| "√" ~ values ~> { (a: MathTerm) ⇒ MathTerm2(Operator.sqrt, a) }
		)
	}


	def funArguments = rule {
		top.+.separatedBy(",")
	}


	/**
	  * 6	ˆ
	  * 5 * / %
	  * 4 + -
	  * 7 :
	  * 6 !
	  * 5 < >
	  * 4 &
	  * 3 ˆ
	  * 2 |
	  * 1 =
	  * 0 slova
	  */
}

class StandardArithParser(inputParser: ParserInput ) extends ArithParser(inputParser){
	override def divMethod: (MathTerm, MathTerm) ⇒ MathTerm = MathTerm3(_: MathTerm, Operator.div, _)

	override def powMethod: (MathTerm, MathTerm) ⇒ MathTerm =  MathTerm3(_: MathTerm, Operator.pow, _)
}


class PowArithParser(inputParser: ParserInput ) extends ArithParser(inputParser) {

	override def divMethod: (MathTerm, MathTerm) ⇒ MathTerm = ConstructorOperator./

	override def powMethod: (MathTerm, MathTerm) ⇒ MathTerm = ConstructorOperator.**
}