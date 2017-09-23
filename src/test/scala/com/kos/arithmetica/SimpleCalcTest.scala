package com.kos.arithmetica

import com.kos.ariphmetica.Calculator._
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by Kos on 25.03.2017.
  */
class SimpleCalcTest extends FlatSpec with Matchers {

	init()

	def checker(a:Seq[(String,String)]): Unit ={
		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"pi" should "" in {
		checker( Seq("pi" → "pi",
			"pi*2" → "2*pi",
			"2*pi" → "2*pi",
			"3*pi/2" → "3*pi/2",
			"3/2*pi" → "3*pi/2",
			"pi/2*3" → "3*pi/2"
		))
	}

	"pow -1" should "" in {

		checker( Seq("x/1-x" → "0",
			"y/(-1)" → "(-1)*y",
			"|8-10|" → "2",
			"(-1)*(3*9)" → "-27",
			"|||4-12|||" → "8"
		))
	}

	"pow n" should "" in {
		checker( Seq("x^(6^(5^y))" → "x^(6^(5^y))",
			"((x^6)^5)^y" → "x^(30*y)",
			"x*3+3*x" → "6*x",
			"(x*4)*(4*x)" → "16*x^2",
			"6^y*6^x" → "6^(x+y)",
			"x^7*x^3" → "x^10",
			"x^2*x^3" → "x^5"

		))
	}

	"pow multiplication" should "" in {
		checker( Seq("(7+x)^(5*8)*sin(x)^3*(x+7)^8*sin(x)^23" → "(7+x)^48*sin(x)^26",
			"(7+x)^(5*8)*sin(x)^3*(x+7)^8" → "(7+x)^48*sin(x)^3",
			"x*x*x*x" → "x^4",
			"x^2*(7*x)*x^(3*7)*y^5*x^y*y^3" → "7*x^(24+y)*y^8"

		))
	}
}
