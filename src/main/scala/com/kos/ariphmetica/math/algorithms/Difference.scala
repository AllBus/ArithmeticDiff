package com.kos.ariphmetica.math.algorithms

import com.kos.ariphmetica.math.{C0, C1, CalcException, Func1, Operator}
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.functions.ArithFun
import com.kos.ariphmetica.math.terms._

/**
  * Дифференцированте функции arg по dx
  * Created by Kos on 21.03.2017.
  */
object Difference {

	/**
	  * Параметр для дифференцирования по шагам
	  *
	  * @param a  - дифференцируемая функция
	  * @param dx - переменная дифференцирования
	  * @return правило дифференцирования
	  */
	def ^@(a: MathTerm, dx: String): MathTerm = DiffTerm(a, dx)

	/**
	  * Параметер для дифференцирования целиком
	  *
	  * @param a  - дифференцируемая функция
	  * @param dx - переменная дифференцирования
	  * @return правило дифференцирования
	  */
	def ^#(a: MathTerm, dx: String): MathTerm = dif(dx, ^#)(a)

	/**
	  * Дифференцированте функции arg по dx
	  *
	  * @param dx  - переменная дифференцирования
	  * @param ^!  - способ выолнения дифференцирования ^@ или ^#
	  * @param arg - дифференцируемая функция
	  * @return arg(dx)'dx
	  */
	def dif(dx: String, ^! : (MathTerm, String) ⇒ MathTerm)(arg: MathTerm): MathTerm = {

		def ^(a: MathTerm) = {
			a match {
				case MathConst(x) ⇒ if (x == dx) C1 else C0
				case _ ⇒ ^!(a, dx)
			}
		}

		def ^^(f: MathTerm, g: MathTerm): MathTerm = {
			if (g == C0)
				C0
			else
				*(g, ^(f))
		}

		def *(f: MathTerm, g: MathTerm): MathTerm = {
			MathTerm3(f, mul, g) match {
				case MathTerm3(ff, `mul`, C1) ⇒ ff
				case MathTerm3(C1, `mul`, gg) ⇒ gg
				case MathTerm3(C0, `mul`, gg) ⇒ C0
				case MathTerm3(ff, `mul`, C0) ⇒ C0
				case r ⇒ r
			}
		}

		arg match {
			case MathTerm3(f, op: Operator, g) ⇒
				val fx = contain(dx)(f)
				val gx = contain(dx)(g)
				if (fx) {
					if (gx) {
						// f(x) g(x)
						op.dif(^(f), ^(g), f, g)
					} else {
						//f(x)
						op.difLeft(^(f), f, g)
					}
				} else {
					if (gx) {
						//g(x)
						op.difRight(^(g), f, g)
					} else {
						// обе части константы
						C0
					}
				}
			case MathTerm2(op: Func1, f) ⇒
				val fx = contain(dx)(f)

				if (!fx) C0	else ^^(f, op.dif(f))

			//			case x: ArithFun ⇒
			//				throw CalcException.undefinedFunction(s"${x.funName}")

			case MathConst(x) ⇒ if (x == dx) C1 else C0
			case f@DiffTerm(x, sdx) ⇒

				val fx = contain(dx)(f)
				if (fx) {
					val r = dif(sdx, ^!)(x)
					dif(dx, ^!)(r)
				} else
					C0

			case _:Digit ⇒ C0 //Digit
//			case x :ArithFun ⇒ UndefinedDiff(x,dx)
//			case x :UndefinedDiff ⇒ x
			case x ⇒ CalcException.differenceNotSupportElement(x)
		}
	}

	/**
	  * Дифференцирование одного шага
	  *
	  * @param ^!  - способ выолнения дифференцирования ^@ или ^#
	  * @param arg - дифференцируемая функция
	  * @return arg(dx)'dx
	  */
	def difOne(^! : (MathTerm, String) ⇒ MathTerm)(arg: MathTerm): MathTerm = {

		def cont(arg: MathTerm): MathTerm = {
			arg match {
				case MathTerm3(f, op, g) ⇒ MathTerm3(cont(f), op, cont(g))
				case MathTerm2(op, f) ⇒ MathTerm2(op, cont(f))
				case DiffTerm(f, dx) ⇒ dif(dx, ^!)(f)
				case x ⇒ x
			}
		}

		cont(arg)
	}
}
