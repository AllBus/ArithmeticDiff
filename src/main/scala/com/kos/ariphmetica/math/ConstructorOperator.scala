package com.kos.ariphmetica.math

import com.kos.ariphmetica.math.Operator.*
import com.kos.ariphmetica.math.terms.{IntDigit, MathTerm, MathTerm2, MathTerm3}

import scala.annotation.targetName

/**
  * Created by Kos on 24.03.2017.
  */
object ConstructorOperator {

	@targetName("div")
	def /(g: MathTerm): MathTerm = {
		№(pow_1, g)
	}

	def /(f: Int, g: MathTerm): MathTerm = {
		IntDigit(f) match {
			case C1 ⇒ (pow_1, g)
			case x ⇒ (f, mul, №(pow_1, g))
		}
	}

	def /(f: MathTerm, g: MathTerm): MathTerm = {
		f match {
			case C1 ⇒ (pow_1, g)
			case x ⇒ (f, mul, №(pow_1, g))
		}
	}

//	/** add */
//	def +(f: MathTerm, g: MathTerm): MathTerm = {
//		val r = MathTerm3(f, add, g)
//		r match {
//			case MathTerm3(C0, `add`, gg) ⇒ gg
//			case MathTerm3(ff, `add`, C0) ⇒ ff
//			case MathTerm3(C1, `add`, C_1) ⇒ C0
//			case MathTerm3(C_1, `add`, C1) ⇒ C0
//			case _ ⇒ r
//		}
//	}

	/** mul */
	def *(f: Int, g: MathTerm): MathTerm = {
		f match {
			case 1 ⇒ g
			case 0 ⇒ C0
			case _ ⇒ MathTerm3(IntDigit(f), mul, g)
		}
	}

	/** mul */
	def *(f: MathTerm, g: MathTerm): MathTerm = {
		val r = MathTerm3(f, mul, g)
		r match {
			case MathTerm3(ff, `mul`, C1) ⇒ ff
			case MathTerm3(C1, `mul`, gg) ⇒ gg
			case MathTerm3(C0, `mul`, gg) ⇒ C0
			case MathTerm3(ff, `mul`, C0) ⇒ C0
			case MathTerm3(C_1, `mul`, C_1) ⇒ C1
			case _ ⇒ r
		}
	}

	/** pow */
	def **(fg: (MathTerm, MathTerm)): MathTerm = {
		**(fg._1,fg._2)
	}

	/** pow */
	def **(f: MathTerm, g: MathTerm): MathTerm = {
		g match {
			case C1 ⇒ f
			case C2 ⇒ MathTerm2(sqr, f)
			case C_1 ⇒ MathTerm2(pow_1, f)
			case C1_2 ⇒ MathTerm2(sqrt, f)
			case MathTerm3(C_1, `mul`, MathTerm2(`pow_1`,C2)) ⇒ MathTerm2(`pow_1`,MathTerm2(`sqrt`,f))
//					case y:Digit if y.lessZero && !y.overNegative  ⇒
//							MathTerm2(pow_1,MathTerm3(f,pow, y.negative))
			case _ ⇒ MathTerm3(f, pow, g)
		}
	}

	def ^^(df: ⇒ MathTerm, g: MathTerm): MathTerm = {
		if (g == C0)
			C0
		else
			*(g, df)
	}

	def ---(f: MathTerm): MathTerm = {
		f match {
			case C1 ⇒ C_1
			case C0 ⇒ C0
			case C_1 ⇒ C_1
			//case x: Int ⇒ -x
			//case x: Double ⇒ -x
			case _ ⇒ (-1, mul, f)
		}
	}

	def --(f: MathTerm, g: MathTerm): MathTerm = {
		g match {
			//				case b: Int ⇒
			//					f match {
			//						case (a, `sub`, c: Int) ⇒
			//							if (c + b == 0)
			//								a
			//							else
			//								(a, sub, c + b)
			//						case c: Int ⇒ c - b
			//						case _ ⇒ (f, sub, g)
			//					}
			case _ ⇒ (f, sub, g)
		}
	}


}
