package com.kos.ariphmetica.math.terms

import com.kos.ariphmetica.Num

/**
  * Created by Kos on 22.03.2017.
  */
class MathDigitError(val name:String,val num:Num) extends Digit {

	def lessZero :Boolean = false
	def overNegative: Boolean = true
//	def overDiv(x: IntDigit):Boolean = true
//	def overMul(x: IntDigit):Boolean = true
//	def overAdd(x: IntDigit):Boolean = true
//	def overSub(x: IntDigit):Boolean = true

	override def orderValue: Int = 3

	override def toString: String = {s"Error: $name"}

	override def >(b: Digit) = false

	override def <(b: Digit) = false

	override def /(b: Digit) = this

	override def %(b: Digit) = this

	override def *(b: Digit) = this

	override def +(b: Digit) = this

	override def -(b: Digit) = this

	override def ^(b:Digit) = this

	override def log(b:Digit) = this

	override def negative = this

	override def sqr: Digit = this

	override def abs: Digit = this

	override def sign: Digit = this
}
