package com.kos.ariphmetica.math.terms

import com.kos.ariphmetica.Num
import com.kos.ariphmetica.math.{C0, C1, C10, C12, C180, C2, C3, C4, C6, C_1, CalcException}

/**
  * Created by Kos on 24.03.2017.
  */
case class FloatDigit(value: Num) extends Digit {

	def lessZero :Boolean = value<0

	override def orderValue: Int = 17
	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:FloatDigit ⇒ value.compare(x.value)
			case x:IntDigit ⇒ value.compare(x.value)
			case _ ⇒ 0
		}
	}

	def overNegative: Boolean = false
//	def overDiv(x: IntDigit):Boolean = true
//	def overMul(x: IntDigit):Boolean = true
//	def overAdd(x: IntDigit):Boolean = true
//	def overSub(x: IntDigit):Boolean = true

	override def toString = {
		if (value>=0)
			value.toString
		else{
			"("+value.toString+")"
		}
	}

	def >(b: Digit) = b match {
		case x: IntDigit ⇒ value > x.value
		case x: FloatDigit ⇒ value > x.value
	}

	def <(b: Digit) = b match {
		case x: IntDigit ⇒ value < x.value
		case x: FloatDigit ⇒ value < x.value
	}

	def /(b: Digit): Digit = b match {
		case _: Digit0 ⇒ CalcException.divisionByZero(value)
		case _: Digit1 ⇒ this
		case x: IntDigit ⇒ !!(value / x.value)
		case x: FloatDigit ⇒ !!(value / x.value)
	}

	def %(b: Digit): Digit = b match {
		case _: Digit0 ⇒ CalcException.divisionByZero(value)
		case x: IntDigit ⇒ !!(value % x.value)
		case x: FloatDigit ⇒ !!(value % x.value)
	}

	def *(b: Digit): Digit = b match {
		case _: Digit0 ⇒ C0
		case _: Digit1 ⇒ this
		case x: IntDigit ⇒ !!(value * x.value)
		case x: FloatDigit ⇒ !!(value * x.value)

	}

	def +(b: Digit): Digit = b match {
		case _: Digit0 ⇒ this
		case x: IntDigit ⇒ !!(value + x.value)
		case x: FloatDigit ⇒ !!(value + x.value)
	}

	def -(b: Digit): Digit = b match {
		case _: Digit0 ⇒ this
		case x: IntDigit ⇒ !!(value - x.value)
		case x: FloatDigit ⇒ !!(value - x.value)
	}

	def ^(b: Digit) = b match {
		case x: IntDigit ⇒ !!(Math.pow(value, x.value))
		case x: FloatDigit ⇒ !!(Math.pow(value, x.value))
	}

	def log(b: Digit) = b match {
		case x: IntDigit ⇒ !!(Math.log(value) / Math.log(x.value))
		case x: FloatDigit ⇒ !!(Math.log(value) / Math.log(x.value))
	}

	def sqr = !!(value*value)

	def abs :Digit  = !!(Math.abs(value))
	def sign :Digit = if (value>0) C1 else if (value<0) C_1 else C0

	def negative = !!(-value)

	def !!(v: Num): Digit = {
		v match {
			case 0 ⇒ C0
			case 1 ⇒ C1
			case -1 ⇒ C_1
			case 2 ⇒ C2
			case 3 ⇒ C3
			case 4 ⇒ C4
			case 6 ⇒ C6
			case 10 ⇒ C10
			case 12 ⇒ C12
			case 180 ⇒ C180
			case _ ⇒ FloatDigit(v)
		}

		//     if (v.toInt == v)
		//       v.toInt
		//     else
		//       v
	}

	override def equals(other: Any): Boolean = {
		other match {
			case a: FloatDigit => value == a.value
			case a: IntDigit => value == a.value
			case a: Int => value == a
			case a: Double => value == a
			case x => toString == x.toString
		}
	}
}

class SubFloatDigit() extends FloatDigit(0){
	override def toString: String = "NAN"

	override def >(b: Digit)= false
	override def <(b: Digit)= false
}