package com.kos.ariphmetica.math.terms

trait Digit extends MathTerm {
	def lessZero :Boolean


	override def orderValue: Int = 10

	//	val value:Any

	def >(b: Digit): Boolean

	def <(b: Digit): Boolean

	def %(b: Digit): Digit

	def /(b: Digit): Digit

	def *(b: Digit): Digit

	def +(b: Digit): Digit

	def -(b: Digit): Digit

	def ^(b: Digit): Digit

	def log(b: Digit): Digit

	def sqr :Digit
	def abs :Digit
	def sign :Digit

	def negative: Digit

	def overNegative: Boolean

//	def overDiv(x: IntDigit):Boolean
//	def overMul(x: IntDigit):Boolean
//	def overAdd(x: IntDigit):Boolean
//	def overSub(x: IntDigit):Boolean
}



