package com.kos.ariphmetica

import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.terms._

import scala.languageFeature.implicitConversions

/**
  * Created by Kos on 20.03.2017.
  */
package object math {

	implicit class ImplMathTerm(val value: Int) extends AnyVal {
		def toDigit: Digit = IntDigit(value)
	}

	//	implicit def intToDigit(value:Int): Digit = Digit(value)

	implicit def tuple2ToMathTerm(value: (Func1, MathTerm)): MathTerm = new MathTerm2(value._1, value._2)

	implicit def tuple3MOIToMathTerm(value: (MathTerm, Operator, Int)): MathTerm = {
		val right = convertIntToTerm(value._3)
		MathTerm3(value._1, value._2, right)
	}

	implicit def tuple3IOMToMathTerm(value: (Int, Operator, MathTerm)): MathTerm = {
		val left = convertIntToTerm(value._1)
		MathTerm3(left, value._2, value._3)
	}

	implicit def tuple3ToMathTerm(value: (MathTerm, Operator, MathTerm)): MathTerm = MathTerm(value)

	def №(left: MathTerm, op: Operator, right: MathTerm) = {
		MathTerm3(left, op, right)
	}

	def №(leftInt: Int, op: Operator, right: MathTerm) = {
		val left = convertIntToTerm(leftInt)
		MathTerm3(left, op, right)
	}

	def №(op: Func1, arg: MathTerm) = {
		MathTerm2(op, arg)
	}

	def convertIntToTerm(leftInt: Long): Digit = {
		IntDigit(leftInt)

	}

	def convertIntToTerm(leftInt: Int): Digit = {
		IntDigit(leftInt)
	}

	def convertStringToTerm(value: String): MathTerm = {
		MathConst(value)
		//		value match {
		//			case 0 ⇒ C0
		//			case 1 ⇒ C1
		//			case 2 ⇒ C2
		//			case -1 ⇒ C_1
		//			case 180 ⇒ C180
		//			case x ⇒ to x
		//		}
	}

	//========================
	def tryToNum(valueText: String): MathTerm = {
		try {
			val value = valueText.replace('−', '-')
			try {
				convertIntToTerm(value.toLong)
			}
			catch {
				case _: Throwable => FloatDigit(value.toDouble)
			}
		}
		catch {
			case e: Throwable => CalcException.undefinedValue(valueText)
		}
	}

	import ConstructorOperator._


}
