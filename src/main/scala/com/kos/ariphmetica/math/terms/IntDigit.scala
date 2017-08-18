package com.kos.ariphmetica.math.terms

import com.kos.ariphmetica.Num
import com.kos.ariphmetica.math.algorithms.IntMath
import com.kos.ariphmetica.math._

/**
  * Created by Kos on 22.03.2017.
  */
class IntDigit(val value: Long) extends Digit {

	def lessZero :Boolean = value<0

	override def orderValue: Int = 15
	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:FloatDigit ⇒ value.toDouble.compare(x.value)
			case x:IntDigit ⇒ value.compare(x.value)
			case _ ⇒ 0
		}
	}

	override def toString: String =
		if (value>=0)
			value.toString
		else{
			"("+value.toString+")"
		}


	override def equals(other: Any): Boolean = {
		other match {
			case a: IntDigit => value == a.value
			case a: FloatDigit => value == a.value
			case a: Int => value == a
			case a: Double => value == a
			case x => toString == x.toString
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
		case x: IntDigit ⇒
			if (x.value == 0)
				CalcException.divisionByZero(value)
			else {
				if (overDiv(x)) {
					!!(value.toDouble / x.value.toDouble)
				} else {
					!!(value / x.value)
				}
			}
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
		case x: IntDigit ⇒
			if (overMul(x))
				!!(value.toDouble * x.value.toDouble)
			else
				!!(value * x.value)


		case x: FloatDigit ⇒ !!(value * x.value)

	}

	def +(b: Digit): Digit = b match {
		case _: Digit0 ⇒ this
		case x: IntDigit ⇒
			val p = value + x.value
			if (value < 0) {
				if (x.value < 0) {
					if (p < 0) !!(p) else !!(value.toDouble + x.value.toDouble)

				} else {
					!!(p)
				}
			} else if (value == 0) {
				x
			} else {
				//a>0
				if (x.value <= 0) {
					!!(p)
				}
				else {
					if (p > 0) !!(p) else !!(value.toDouble + x.value.toDouble)
				}
			}

		case x: FloatDigit ⇒ !!(value + x.value)
	}

	def -(b: Digit): Digit = b match {
		case _: Digit0 ⇒ this
		case x: IntDigit ⇒
			val p = value - x.value
			if (value < 0) {
				if (x.value > 0) {
					if (p < 0) !!(p) else !!(value.toDouble - x.value.toDouble)
				} else {
					!!(p)
				}
			} else if (value == 0) {
				!!(p)
			} else {
				//a>0
				if (x.value >= 0) {
					!!(p)
				}
				else {
					if (p > 0) !!(p) else !!(value.toDouble - x.value.toDouble)
				}
			}

		case x: FloatDigit ⇒ !!(value - x.value)
	}

	def ^(b: Digit) = b match {
		case x: IntDigit ⇒ !!(Math.pow(value, x.value))
		case x: FloatDigit ⇒ !!(Math.pow(value, x.value))
	}

	def log(b: Digit) = b match{
		case x: IntDigit ⇒ !!(Math.log(value) / Math.log(x.value))
		case x: FloatDigit ⇒ !!(Math.log(value) / Math.log(x.value))
	}

	def sqr = {
		if (Math.abs(value)<Int.MaxValue)
			!!(value*value)
		else
			!!(value.toDouble*value.toDouble)
	}

	override def negative = if (!overNegative) !!(-value) else !!(-value.toDouble)
	override def abs :Digit  = if (!overNegative) !!(Math.abs(value)) else !!(Math.abs(value.toDouble))
	override def sign :Digit = if (value>0) C1 else if (value<0) C_1 else C0



	def overNegative: Boolean = value==Long.MinValue
	def overDiv(x: IntDigit):Boolean = x.value==0 || value % x.value != 0
	def overMul(x: IntDigit):Boolean = !(
		IntMath.highestBitAbs(value) + IntMath.highestBitAbs(x.value)<64
	 )

	def overAdd2(x:IntDigit):Boolean ={
		!(IntMath.highestBit(Math.abs(value)|Math.abs(x.value))<63)
	}

	def overAdd(x: IntDigit):Boolean = {
		val p = value + x.value
		if (value < 0) {
			if (x.value < 0) p >= 0 else false
		} else if (value == 0) {
			false
		} else { //a>0
			if (x.value <= 0) false	else p <= 0
		}
	}

	def overSub(x: IntDigit):Boolean = {
		val p = value - x.value
		if (value < 0) {
			if (x.value > 0) p >= 0 else false
		} else if (value == 0) {
			false
		} else {	//a>0
			if (x.value >= 0) false	else p <= 0
		}
	}

	def !!(v: Long): Digit = {
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
			case _ ⇒ new IntDigit(v)
		}
	}

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

}


object IntDigit {
	def apply(num: Long): IntDigit = num match {
		case 0 ⇒ C0
		case 1 ⇒ C1
		case 2 ⇒ C2
		case -1 ⇒ C_1
		case 10 ⇒ C10
		case 180 ⇒ C180
		case x ⇒ new IntDigit(x)
	}
}

class SubIntDigit() extends IntDigit(0){
	override def toString: String = "NAN"

	override def >(b: Digit)= false
	override def <(b: Digit)= false
}

class Digit0() extends IntDigit(0) {


	override def toString: String = "0"

	override def %(b: Digit) = this

	override def *(b: Digit) = this

	override def /(b: Digit): Digit = {
		b match {
			case x: Digit0 ⇒ CalcException.divisionByZero(x.value)
			case x ⇒ super./(x)
		}
	}

	override def +(b: Digit) = b

	override def -(b: Digit) = b.negative

	//override def ^(b:Digit) = !!(Math.pow(value, b.value))

	//override def log(b:Digit) = !!(Math.log(value) / Math.log(b.value))

	override def sqr = this
	override def abs :Digit  = this
	override def sign :Digit = this
	override def negative = this


	override def equals(other: Any): Boolean = {
		other match {
			case x: Digit0 ⇒ true
			case x ⇒ super.equals(other)
		}
	}

	override def overNegative: Boolean = false
}

class Digit1() extends IntDigit(1) {

	override def toString: String = "1"

	override def /(b: Digit): Digit = super./(b)

	override def %(b: Digit): Digit = super.%(b)

	override def *(b: Digit): Digit = b

	override def +(b: Digit): Digit = b match {
		case _: Digit_1 ⇒ C0
		case _ ⇒ super.+(b)
	}

	override def -(b: Digit): Digit =
		b match {
			case x: Digit1 ⇒ C0
			case _ ⇒ super.-(b)
		}

	override def ^(b: Digit): Digit = this

	//override def log(b: Digit): Digit = super.log(b)

	override def sqr = this
	override def abs :Digit  = this
	override def sign :Digit = this
	override def negative: Digit = C_1

	override def equals(other: Any): Boolean = {
		other match {
			case x: Digit1 ⇒ true
			case _ => super.equals(other)
		}
	}
	override def overNegative: Boolean = false
}

class Digit_1() extends IntDigit(-1) {



	override def toString: String = "(-1)"

	override def +(b: Digit): Digit = b match {
		case _: Digit1 ⇒ C0
		case _ ⇒ super.+(b)
	}

	override def -(b: Digit): Digit =
		b match {
			case x: Digit_1 ⇒ C0
			case _ ⇒ super.-(b)
		}

	override def sqr = C1
	override def abs :Digit  = C1
	override def sign :Digit = this
	override def negative: Digit = C1

	override def equals(other: Any): Boolean = {
		other match {
			case x: Digit_1 ⇒ true
			case _ => super.equals(other)
		}
	}
	override def overNegative: Boolean = false
}