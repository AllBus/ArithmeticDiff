package com.kos.arithmetica

import com.kos.ariphmetica.Calculator._
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by Kos on 23.03.2017.
  */
class DiffTest extends FlatSpec with Matchers {

	init()

	"function f1(x)" should "" in {
		diff("(y*y)'y") should equal(res("y+y"))
	}

	"function f2(x)" should "" in {
		diff("(sin(x))'x") should equal(res("cos(x)"))
	}

	"function f3(x)" should "" in {
		calc("(e^(2*x))'x'x") should equal(res("4*e^(2*x)"))
	}


	"function f4(x)" should "" in {
		calc("(ln(7*x))'x") should equal(res("1/x"))
	}


	"function f5(x)" should "" in {
		calc("(sin(x+y)*cos(x))'x'y") should equal(res("-(sin(x)∙cos(x+y)+sin(x+y)∙cos(x))"))
	}
	"function f6(x)" should "" in {
		calc2("(4*x+9*x*x*12+4+8)'x") should equal(res("216*x+4"))
	}
	"trigonometry" should "" in {

		val a=Seq("sin(3*pi/2)"→ "-1",
		"sin(pi)" → "0",
		"sin(pi/2)"→ "1",
		"sin(pi/3)"→ "sqrt(3)/2",
		"3*sin(pi/4)"→ "3/sqrt(2)",
		"3*sin(pi/6)"→ "3/2",
		"sin(0)"→ "0",

		"tg(pi)"→ "0",
		"tg(pi/3)"→ "sqrt(3)",
		"tg(pi/4)"→ "1",
		"8*tg(pi/6)"→ "8/sqrt(3)",
		"tg(0)" → "0",

		"cos(3*pi/2)" → "0",
		"cos(pi)" → "-1",
		"cos(pi/2)" → "0",
		"7*cos(pi/3)" → "7/2",
		"cos(pi/4)*7" → "7/sqrt(2)",
		"cos(pi/6)" → "sqrt(3)/2",
		"cos(0)" → "1",

		"ctg(3*pi/2)" → "0",
		"ctg(pi/2)" → "0",
		"ctg(pi/3)*8" → "8/sqrt(3)",
		"ctg(pi/4)*8" → "8",
		"ctg(pi/6)" → "sqrt(3)")

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"divide" should "" in {
		val a=Seq("-1/x" → "x^-2",
			"-7/(x+3)" → "(3+x)^-2*7",
			"-1/(4*x+3)" → "4*(3+4*x)^-2")

		for ((f,r) ← a) {
			calc2("("+f+")'x") should equal(res(r))
		}
	}

	"function x^2" should "" in {
		val a=Seq(
			"x^2" → "2*x",
			"sin(x^2)" → "2*x*cos(x^2)")
		for ((f,r) ← a) {
			calc("("+f+")'x") should equal(res(r))
		}
	}

}


