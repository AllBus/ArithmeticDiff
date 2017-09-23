package com.kos.ariphmetica.math.algorithms

import com.kos.ariphmetica.Num
import com.kos.ariphmetica.math._
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.terms.compose.ComposeTerm

/**
  * Created by Kos on 22.03.2017.
  */
object Calculate {

	def !!(x: Num): Digit = FloatDigit(x)

	private[this] def digit(a: MathTerm): MathTerm = {
		a match {
			case _ ⇒ a
		}
	}


	/**
	  * Вычисление только того что может быть вычислено без потерь
	  */
	def simpleCalc(arg: MathTerm): MathTerm = {

		def ^ = simpleCalc _

		arg match {

			case MathTerm3(f, op: Operator, g) ⇒
				(digit(^(f)), digit(^(g))) match {


					case (x, y: Digit0) ⇒
						op match {
							case `add` ⇒ x
							case `sub` ⇒ x
							case `mul` ⇒ C0
							case `div` ⇒ CalcException.divisionByZero(0)
							case `pow` ⇒ C1 //Всё что угодно в степени 0 равно 1 (x^0 = 1)
							case _ ⇒ MathTerm3(x, op, y)
						}

					case (x: Digit0, y) ⇒
						op match {
							case `add` ⇒ y
							case `sub` ⇒ y match {
								case b: Digit  if !b.overNegative  ⇒ b.negative
								case _ ⇒ (neg, y)
							}

							case `mul` ⇒ C0
							case `pow` ⇒ y match {
								case C0 ⇒ C1
								case z:IntDigit ⇒
									if (z.value>0) C0 else if (z.value == 0) C1 else MathTerm3(x, op, y)
								case _ ⇒ MathTerm3(x, op, y)
							}
							//case `div` ⇒ MathTerm3(x, op, y)
							case _ ⇒ MathTerm3(x, op, y)
						}
					case (x: Digit_1, y: Digit1) ⇒
						op match {
							case `add` ⇒ C0
							case `sub` ⇒ (neg, C2)
							case `mul` ⇒ C_1
							case `div` ⇒ C_1
							case `pow` ⇒ C_1
							case `mod` ⇒ x % y
							case _ ⇒ MathTerm3(x, op, y)
						}
					case (x: Digit1, y: Digit_1) ⇒
						op match {
							case `add` ⇒ C0
							case `sub` ⇒ C2
							case `mul` ⇒ C_1
							case `div` ⇒ C_1
							case `pow` ⇒ C1
							case `mod` ⇒ x % y
							case _ ⇒ MathTerm3(x, op, y)
						}
					case (x: IntDigit, y: IntDigit) ⇒
						op match {
							case `add` if !x.overAdd(y) ⇒ x + y
							case `sub` if !x.overSub(y) ⇒ x - y
							case `mul` if !x.overMul(y) ⇒ x * y
							case `div` if !x.overDiv(y) ⇒ x / y
							case `mod` ⇒ x % y
							case `pow` ⇒ y match {
								case C1 ⇒ x
								case C_1 ⇒ (C1, div, x)
								case C0 ⇒ C1
								case z if !IntMath.overPow(x.value,z.value) ⇒ IntDigit(IntMath.pow(x.value,z.value))
								case _ ⇒ MathTerm3(x, op, y)
							}
							case _ ⇒ MathTerm3(x, op, y)
						}

					case (xv@MathTerm2(`neg`,x), yv@MathTerm2(`neg`,y)) ⇒
						op match {
							case `mul` => MathTerm3(x, mul, y )
							case `add` ⇒ (neg ,MathTerm3(x, op, y))
							case `sub` ⇒ (neg ,MathTerm3(x, op, y))
							case _ ⇒ MathTerm3(xv, op, yv)
						}

					case (x,yv@MathTerm2(`neg`,y)) ⇒
						op match {
							case `mul` ⇒
								x match {
									case xv:Digit if !xv.overNegative => MathTerm3(xv.negative, mul, y )
									case _ ⇒ (neg, MathTerm3(x, mul,y) )
								}

							case `add` ⇒ MathTerm3(x, sub, y)
							case `sub` ⇒ MathTerm3(x, add, y)
							case _ ⇒ MathTerm3(x, op, yv)
						}
					case (xv@MathTerm2(`neg`,x), y) ⇒
						op match {
							case `mul` ⇒
								y match{
									case yv:Digit if !yv.overNegative  => MathTerm3(x, mul, yv.negative )
									case _ ⇒ (neg, MathTerm3(x, mul,y))
								}

							case `add` ⇒ MathTerm3(y, sub, x)
							case `sub` ⇒ (neg ,MathTerm3(x, add, y))
							case _ ⇒ MathTerm3(xv, op, y)
						}

					case (x, y: Digit1) ⇒
						op match {
							case `mul` ⇒ x
							case `div` ⇒ x
							case `pow` ⇒ x

							case _ ⇒ MathTerm3(x, op, y)
						}
					case (x: Digit1, y) ⇒
						op match {
							case `mul` ⇒ y
							case `pow` ⇒ C1 // 1^y = 1
							case _ ⇒ MathTerm3(x, op, y)
						}


					case (x, y) ⇒ MathTerm3(x, op, y)
				}
			case MathTerm2(op: Func1, f) ⇒
				digit(^(f))	match {
					case x: Digit0 ⇒
						op match {

							case `sqr` ⇒ C0
							case `sqrt` ⇒ C0
							case `neg` ⇒ C0
							case `abs` ⇒ C0
							case `sgn` ⇒ C0
							case `exp` ⇒ C1

							case `arcsin` ⇒ C0
							case `arccos` ⇒ pi_2
							case `arctg` ⇒ C0
							case `arcctg` ⇒ pi_2

							case `sh` ⇒ C0
							case `ch` ⇒ C1
							case `th` ⇒ C0

							case `arsh` ⇒ C0
							case `arth` ⇒ C0

							case _ ⇒ TrigonometryCalc.calc( MathTerm2(op, x))
						}
					case x: Digit1 ⇒
						op match {
							case `pow_1`⇒ C1
							case `ln` ⇒ C0
							case `lg` ⇒ C0
							case `sqr` ⇒ C1
							case `sqrt` ⇒ C1
							case `neg` ⇒ C_1
							case `abs` ⇒ C1
							case `sgn` ⇒ C1
							case `exp` ⇒ e

							case `arcsin` ⇒ pi_2
							case `arccos` ⇒ C0
							case `arctg` ⇒ pi_4
							case `arcctg` ⇒ pi_4

							case `arch` ⇒ C0

							case _ ⇒  TrigonometryCalc.calc( MathTerm2(op, x))
						}
					case x: Digit_1 ⇒
						op match {
							case `pow_1`⇒ C_1
							case `sqr` ⇒ C1
							case `sqrt` ⇒ IMPL_1
							case `neg` ⇒ C1
							case `abs` ⇒ C1
							case `sgn` ⇒ C_1

							case `exp` ⇒ (C1, div, e)

							case `arcsin` ⇒ (neg, pi_2)
							case `arccos` ⇒ pi
							case `arctg` ⇒ (neg, pi_4)
							case `arcctg` ⇒ (C3, mul, pi_4)

							case _ ⇒  TrigonometryCalc.calc( MathTerm2(op, x))
						}

					case x:IntDigit ⇒
						op match {
							case `sqr`if !x.overMul(x) ⇒  x * x
							case `abs`if !x.overNegative ⇒ x.abs
							case `neg`if !x.overNegative ⇒ x.negative
							case `sqrt` ⇒
								val a=Math.sqrt(x.value).toLong
								if (a*a == x.value){
									IntDigit(a)
								}else {
									MathTerm2(op, x)
								}
							case _ ⇒ TrigonometryCalc.calc( MathTerm2(op, x))
						}
					case x@`e` ⇒
						op match {
							case `ln` ⇒ C1
							case _ ⇒ MathTerm2(op, x)
						}

					case x@MathTerm2(`sqr`, ff) ⇒
						op match {
							case `sqr` ⇒ MathTerm3(ff, pow, C4)
							case _ ⇒ TrigonometryCalc.calc(MathTerm2(op, x))
						}
					case x@MathTerm2(`neg`, ff) ⇒
						op match {
							case `neg` ⇒ ff
							case _ ⇒ TrigonometryCalc.calc(MathTerm2(op, x))
						}

					case x ⇒ TrigonometryCalc.calc( MathTerm2(op, x))
				}

			case DiffTerm(f,dx:String) ⇒ DiffTerm(digit(^(f)),dx)
			case x: ComposeTerm ⇒ digit(^(x.fold))
			case x ⇒ x
		}
	}

	/**
	  * Вычисление значения функции
	  *
	  * @param arg - вычисляемая функция
	  * @return значение функции (также является функцией)
	  */
	def calc(arg: MathTerm): MathTerm = {

		def ^ = calc _

		arg match {
			case MathTerm3(f, op: Operator, g) ⇒
				(digit(^(f)), digit(^(g))) match {
					case (x: Digit, y: Digit) ⇒
						op match {
							case `add` ⇒ x + y
							case `sub` ⇒ x - y
							case `mul` ⇒ x * y
							case `div` ⇒ x / y
							case `pow` ⇒ x ^ y //
							case `mod` ⇒ x % y
							case `log` ⇒ x.log(y)
							case _ ⇒ MathTerm3(x, op, y)
						}
					case (x, y: Digit0) ⇒
						op match {
							case `add` ⇒ x
							case `sub` ⇒ x
							case `mul` ⇒ C0
							case `div` ⇒ CalcException.divisionByZero(0)
							case `pow` ⇒ C1 //Всё что угодно в степени 0 равно 1 (x^0 = 1)
							case _ ⇒ MathTerm3(x, op, y)
						}
					case (x: Digit0, y) ⇒
						op match {
							case `add` ⇒ y
							case `sub` ⇒ (neg, y)
							case `mul` ⇒ C0
							//case `div` ⇒ MathTerm3(x, op, y)
							case _ ⇒ MathTerm3(x, op, y)
						}

					case (x, y: Digit1) ⇒
						op match {
							case `mul` ⇒ x
							case `div` ⇒ x
							case `pow` ⇒ x
							case _ ⇒ MathTerm3(x, op, y)
						}
					case (x: Digit1, y) ⇒
						op match {
							case `mul` ⇒ y
							case `pow` ⇒ C1 // 1^y = 1
							case _ ⇒ MathTerm3(x, op, y)
						}

					case (x, y) ⇒ MathTerm3(x, op, y)
				}
			case MathTerm2(op: Func1, f) ⇒
				digit(^(f)) match {
					case x: FloatDigit ⇒
						op match {
							case `sin` ⇒ !!(Math.sin(x.value))
							case `cos` ⇒ !!(Math.cos(x.value))
							case `tg` ⇒ !!(Math.tan(x.value))
							case `ctg` ⇒ !!(Math.cos(x.value) / Math.sin(x.value))

							case `ln` ⇒ !!(Math.log(x.value))
							case `lg` ⇒ !!(Math.log10(x.value))
							case `sqrt` ⇒ !!(Math.sqrt(x.value))
							case `pow_1` ⇒ !!(1.0/x.value)
							case `sqr` ⇒ x.sqr
							case `neg` ⇒ x.negative
							case `abs` ⇒ x.abs
							case `sgn` ⇒ x.sign
							///					case `pow0` ⇒ 1
							case `exp` ⇒ !!(Math.pow(Math.E, x.value))

							case `arcsin` ⇒ !!(Math.asin(x.value))
							case `arccos` ⇒ !!(Math.acos(x.value))
							case `arctg` ⇒ !!(Math.atan(x.value))
							case `arcctg` ⇒ !!(Math.PI / 2 - Math.atan(x.value))

							case `sh` ⇒ !!(Math.sinh(x.value))
							case `ch` ⇒ !!(Math.cosh(x.value))
							case `th` ⇒ !!(Math.tanh(x.value))
							case `cth` ⇒ !!(1 / Math.tanh(x.value))

							case `arsh` ⇒ !!(Math.log(x.value + Math.sqrt(x.value * x.value + 1)))
							case `arch` ⇒ !!(Math.log(x.value + Math.sqrt(x.value * x.value - 1)))
							case `arth` ⇒ !!(0.5 * Math.log((1.0 + x.value) / (1.0 - x.value)))
							case `arcth` ⇒ !!(0.5 * Math.log((x.value + 1.0) / (x.value - 1.0)))

							case `deg` ⇒ !!(Math.toDegrees(x.value))
							case `rad` ⇒ !!(Math.toRadians(x.value))
							case _ ⇒ MathTerm2(op, x)
						}
					case x: IntDigit ⇒
						op match {
							case `sin` ⇒ !!(Math.sin(x.value))
							case `cos` ⇒ !!(Math.cos(x.value))
							case `tg` ⇒ !!(Math.tan(x.value))
							case `ctg` ⇒ !!(Math.cos(x.value) / Math.sin(x.value))

							case `ln` ⇒ !!(Math.log(x.value))
							case `lg` ⇒ !!(Math.log10(x.value))
							case `sqrt` ⇒ !!(Math.sqrt(x.value))
							case `pow_1` ⇒ !!(1.0/x.value)
							case `sqr` ⇒ x.sqr
							case `neg` ⇒ x.negative
							case `abs` ⇒ x.abs
							case `sgn` ⇒ x.sign
							case `exp` ⇒ !!(Math.pow(Math.E, x.value))

							case `arcsin` ⇒ !!(Math.asin(x.value))
							case `arccos` ⇒ !!(Math.acos(x.value))
							case `arctg` ⇒ !!(Math.atan(x.value))
							case `arcctg` ⇒ !!(Math.PI / 2 - Math.atan(x.value))

							case `sh` ⇒ !!(Math.sinh(x.value))
							case `ch` ⇒ !!(Math.cosh(x.value))
							case `th` ⇒ !!(Math.tanh(x.value))
							case `cth` ⇒ !!(1 / Math.tanh(x.value))

							case `arsh` ⇒ !!(Math.log(x.value + Math.sqrt(x.value * x.value + 1)))
							case `arch` ⇒ !!(Math.log(x.value + Math.sqrt(x.value * x.value - 1)))
							case `arth` ⇒ !!(0.5 * Math.log((1.0 + x.value) / (1.0 - x.value)))
							case `arcth` ⇒ !!(0.5 * Math.log((x.value + 1.0) / (x.value - 1.0)))

							case `deg` ⇒ !!(Math.toDegrees(x.value))
							case `rad` ⇒ !!(Math.toRadians(x.value))
							case _ ⇒ MathTerm2(op, x)
						}

					case x ⇒ MathTerm2(op, x)
				}
			case DiffTerm(f,dx:String) ⇒ DiffTerm(digit(^(f)),dx)
			case x: ComposeTerm ⇒ digit(^(x.fold))
			case x ⇒ x
		}
	}

}
