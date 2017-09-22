package com.kos.arithmetica

import org.scalatest.{FlatSpec, Matchers}

import com.kos.ariphmetica.Calculator._

class CompositeTest extends FlatSpec with Matchers {
	init()

	"function pow" should "" in {
		val a=Seq("3^2^2^2" -> "6561",
		"x^7*x^12" -> "x^19",
		"√(x^2)" -> "x",
		"(√(x))^2" -> "x",
		"x/x*x" -> "x",
		"x/(x*x*x)" -> "x^-2",
		"x^(12-9)/x^(6*3)" -> "x^-15",
		"x^(7+9)*x^(sin(12))*y*x^(11-3)" -> "y*x^(24+sin(12))"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"function sqrt" should "" in {
		val a=Seq(
			"√(90-9)" → "9",
			"√90000" → "300"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"function sqrt digits" should "" in {

		for (v ← 1000L to (1000000000L,2013L)) {
			val f =s"√${v*v}"
			val r= v.toString
			calc2(f) should equal(res(r))
		}
	}

	"parse minus values" should "" in {

		val a=Seq(
			"9-√(25)" → "4",
			"(5-|12-300|)*-1" → "283",
			"5-(12-300)" → "293"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}

	}

	"function simpleCalc MathTerm3 with neg" should "" in {

		val a=Seq(
			"-x*(-y)" → "x*y",
			"45*(-(5+5))*-10" → "4500",
			"-x*y" → "-x*y",
			"-x+(-y)" → "-(x+y)",
			"x+(-y)" → "x-y",
			"-x+234" → "234-x",
			"-x-(-y)" → "-(x-y)",
			"-x+(-y)" → "-(x+y)",
			"-x-y" → "-(x+y)",
			"-x+y" → "(y-x)",
			"x-(-y)" → "(x+y)",
			"x+(-y)" → "(x-y)"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"function minus" should "" in {
		val a=Seq("-9*-10" → "90",
		"-8-12-(-100)" → "80",
		"-x+7" → "7-x",
		"-4-(5-9)-19-20+1039" → "1000",
		//"-4*(7-x)" -> "-28+4*x",
		"9-2-20+100-3000+4000" → "1087",
		"-|-4|+567" → "563",
		"-|-x|" → "-|-x|",
		"-cos(pi)" → "1",
		"-x-7" → "-(7+x)",
		"-98+98" → "0",
		"-(-(-(-(-12+700)*10+400)))" → "6480",
		"√900-(-36)" → "66"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"pow composite" should "" in {
		val a=Seq("a^4*a^9" → "a^13",
			"(1^2*3^4*7^7)^11" → "66706983^11",
			//"(a^2*b^4*c^7)^11" -> "a^22*b^44*c^77",
			"(6^7)^8" → "6^56"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}
}
