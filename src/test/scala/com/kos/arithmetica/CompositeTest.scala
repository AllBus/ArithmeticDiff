package com.kos.arithmetica

import org.scalatest.{FlatSpec, Matchers}
import com.kos.ariphmetica.Calculator._
import com.kos.ariphmetica.math.terms.IntDigit

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
		"x^(7+9)*x^(sin(12))*y*x^(11-3)" -> "x^(24+sin(12))*y",
		"5^4/5^2" → "25",
		"√(x^7)" → "x^(1/2*7)",
		"12^(-(9-7))*12^(9*8)" → "12^(70)"
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
			//"(1^2*3^4*7^7)^11" → "66706983^11",
			//"(a^2*b^4*c^7)^11" -> "a^22*b^44*c^77",
			"(6^7)^8" → "6^56"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"mul composite" should "" in {
		val a=Seq("7*8*9*x*4*a" → "2016∙a∙x"
			//"12*x/4" → "4∙x",
			//"7*x/4" → "1.75∙x",
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"minus composite" should "" in {
		calc2("-9-9-11-23-45")  should equal(IntDigit(-97))
	}

	"sum composite" should "" in {
		val a=Seq(

			"a+b+e+c+q+c+r"→"2*c+a+b+e+q+r",
			"a+b-c-e-g+h+r-f"→"a+b+h+r−c−e−f−g",
			"a+b-c-e-g+h+r-f-b"→"a+h+r−c−e−f−g",
			"a+b-c-e-g+h+r-f-b-d-t-a"→"h+r−c−d−e−f−g−t",
			"a+b-(c+e)-(u-h)+(q-r)"→"a+b+h+q−c−e−r−u",
			"a-b-d-c"→"a-b-c-d",
			"a+b-(c-(e-h-r+(x1+x2+(y1-y2-(z1-z2-z3))-p-(p1+p2-p3+p4+(q1+q2)-(t1-t2)))-x5)+w-k)-u"→
				"a+b+e+k+p3+t1+x1+x2+y1+z2+z3-c-h-p-p1-p2-p4-q1-q2-r-t2-u-w-x5-y2-z1"
		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"pow mul" should "" in {
		val a=Seq(
			"7*(x^3*y^7)^84" → "7∙x^252∙y^588",
			"(y^7*x^3)^12" -> "x^36*y^84",
			"(y^7+x^3)^12" -> "(y^7+x^3)^12",
			"(x^3*c*6)^7" → "279936∙c^7∙x^21"

		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"add add" should "" in {
		val a=Seq(
			"x+x+x+x+x" → "5*x",
			"x+y+x+z+x+x+y+x" → "2*y+5*x+z",
		//	"7*x+x+y+x" → "9*x+y",
		//	"7*x+x+y+7*x" → "15*x+y",
		//	"-x-x-x-x-x" → "-5*x",
			"x-x-x-x-x" → "-3*x"

		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"add sub" should "" in {
		val a=Seq(
			"x+y-x" → "y",
			"2*x-4*y-x*2" → "-(4∙y)"
			//"2*x-4*y-(7-5)*x" → "-(4∙y)"

		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}

	"diff" should "" in {
		val a=Seq(
			"(a*c*x*q)'x" → "a*c*q",
			"(a(x)*b(x))'x" →"a'(x)*b(x)+a(x)*b'(x)",
			"(8+a(x)+b(x))'x" → "a'(x)+b'(x)",
			"(a(x)+sin(3*x)+c(4*x)+d(x))'x" → "a'(x)+3*cos(3*x)+4*c'(4*x)+d'(x)",
			"(a(x+2)*b(x)*c(x))'" → "c'(x)∙a(2+x)∙b(x)+b'(x)∙a(2+x)∙c(x)+a'(2+x)∙b(x)∙c(x)",
			"(a(x*5)*b(x)*c(x))'" → "c'(x)∙a(5∙x)∙b(x)+b'(x)∙a(5∙x)∙c(x)+5∙a'(5∙x)∙b(x)∙c(x)"
			//"(a(x)*b(x)*c(x)*d(x))'x" → "a'(x)∙b(x)∙c(x)∙d(x)+b'(x)∙a(x)∙c(x)∙d(x)+c'(x)∙a(x)∙b(x)∙d(x)+d'(x)∙a(x)∙b(x)∙c(x)"


		)

		for ((f,r) ← a) {
			calc2(f) should equal(res(r))
		}
	}
}
