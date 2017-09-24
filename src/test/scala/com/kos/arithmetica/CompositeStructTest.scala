package com.kos.arithmetica

import com.kos.ariphmetica.Calculator.init
import org.scalatest.{FlatSpec, Matchers}
import com.kos.ariphmetica.Calculator._
import com.kos.ariphmetica.math.terms.MathTerm
import com.kos.ariphmetica.math.terms.compose.{MulTerm, PlusTerm}

class CompositeStructTest extends FlatSpec with Matchers {
	init()


	def as(term:MathTerm):String = term match {
		case x : MulTerm ⇒ x.termsString
		case x : PlusTerm ⇒ x.termsString
		case _ ⇒ "No MulTerm"
	}


	"Pow" should "" in {
		as(parseCompose("a*b*c^4*b")) should equal("*{a b→(1 + 1) c→4}")
	}

	"Pow2" should "" in {
		as(parseCompose("7*9*10*x*11*y")) should equal("*{7 9 10 11 x y}")
	}


}
