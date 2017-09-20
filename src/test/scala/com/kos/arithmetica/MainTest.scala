package com.kos.arithmetica

import com.kos.ariphmetica.math.Operator
import com.kos.ariphmetica.math.algorithms.CopositeFunction
import com.kos.ariphmetica.math.algorithms.classic.ClassicOperator
import com.kos.ariphmetica.math.terms.MathConst
import org.scalatest.{FlatSpec, Matchers}
/**
  * Created by Kos on 23.03.2017.
  */

class MainTest extends FlatSpec with Matchers {

	"A sum" should "two value" in {
		2+4 should be (6)

	}

	"Array" should "two element" in {
		Array(1, 2) should equal(Array(1, 2))

		Array(1, 2).reverse should equal(Array(2, 1))
	}

	"Function" should "pow_1 as div" in {

		Operator.setupPowDiff()

		val pf= for (f ← Operator.func)yield{
			 f → f.dif
		}

		ClassicOperator.setupClassicDiff()

		val x = MathConst("x")
		for (f ← pf){

			f._1.dif(x) should equal(CopositeFunction.replacePowToDiv( f._2(x)))

		}
	}

	"Function" should "div as pow_1" in {

		Operator.setupPowDiff()

		val pf= for (f ← Operator.func)yield{
			f → f.dif
		}

		ClassicOperator.setupClassicDiff()

		val x = MathConst("x")
		for (f ← pf){
			CopositeFunction.replaceDivToPow(f._1.dif(x)) should equal(f._2(x))
		}
	}

	"Operator" should "pow_1 as div" in {

		Operator.setupPowDiff()

		val pf= for (f ← Operator.opers)yield{
			(f , f.dif , f.difLeft , f.difRight)
		}

		ClassicOperator.setupClassicDiff()

		val x = MathConst("x")
		val y = MathConst("y")
		val xs = MathConst("xs")
		val ys = MathConst("ys")

		for (f ← pf){

			f._1.dif(xs,ys,x,y) should equal(CopositeFunction.replacePowToDiv( f._2(xs,ys,x,y)))
			f._1.difLeft(xs,x,y) should equal(CopositeFunction.replacePowToDiv( f._3(xs,x,y)))
			f._1.difRight(ys,x,y) should equal(CopositeFunction.replacePowToDiv( f._4(ys,x,y)))

		}
	}

	"Operator" should "div as pow_1" in {

		Operator.setupPowDiff()

		val pf= for (f ← Operator.opers)yield{
			(f , f.dif , f.difLeft , f.difRight)
		}

		ClassicOperator.setupClassicDiff()

		val x = MathConst("x")
		val y = MathConst("y")
		val xs = MathConst("xs")
		val ys = MathConst("ys")

		for (f ← pf){
			CopositeFunction.replaceDivToPow(f._1.dif(xs,ys,x,y)) should equal(f._2(xs, ys, x, y))
			CopositeFunction.replaceDivToPow(f._1.difLeft(xs,x,y)) should equal(f._3(xs, x, y))
			CopositeFunction.replaceDivToPow(f._1.difRight(ys,x,y)) should equal(f._4(ys, x, y))
		}
	}

}
