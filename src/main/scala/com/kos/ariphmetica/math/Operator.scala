package com.kos.ariphmetica.math

import com.kos.ariphmetica.Num
import com.kos.ariphmetica.math.algorithms.Difference
import com.kos.ariphmetica.math.functions.{ArithFun}
import com.kos.ariphmetica.math.terms._

//
//case class ^^^(val x: Any) {
//
//	override def toString() = s"$x'"
//
//	def toString(dx: String) = s"$x'($dx)"
//}

object Operator {

	import ConstructorOperator._

	def apply(name: String): Operator = {
		name match {
			case "+" ⇒ add
			case "-" | "−" ⇒ sub
			case "/" | "÷" ⇒ div
			case "*" | "×" | "∙" ⇒ mul
			case "^" ⇒ pow
			case _ ⇒ empty
		}
	}

	def calculate = 0 // ** _

	//	def **(arg: MathTerm): MathTerm = {
	//		arg match {
	//			case (x, op: Operator, y) ⇒ Digit(op.calc(x, y))
	//			case (op: Func1, x) ⇒		Digit(op.calc(x))
	//			//	case x: ArithFun ⇒		Digit(x.calc())
	//			case x: Digit ⇒ 	x
	//		//	case x: Int ⇒ 		Digit(x)
	//		//	case x: Double ⇒ 	Digit(x)
	//
	//		//	case _ ⇒ Digit(0)
	//		}
	//	}

	/**
	  * Проверить что функция содержит переменную дифференцирования
	  *
	  * @param param переменная дифференцирования
	  * @param arg   функция
	  * @return
	  */
	def contain(param: String)(arg: MathTerm): Boolean = {
		def cont = contain(param) _

		arg match {
			case MathConst(x) ⇒ x == param
			case MathTerm3(C0, `mul`, g) ⇒ false
			case MathTerm3(f, `mul`, C0) ⇒ false
			case MathTerm3(f, _, g) ⇒ cont(f) || cont(g)
			case MathTerm2(_, f) ⇒ cont(f)
			case DiffTerm(_ ,dx) if dx == param ⇒ true
			case DiffTerm(mathTerm,_) ⇒ cont(mathTerm)
			case UndefinedDiff(_,dx) if dx == param ⇒ true
			case UndefinedDiff(mathTerm,_) ⇒ cont(mathTerm)
			//case OneFun(_,f) ⇒ cont(f)
			case x: ArithFun ⇒ x.args.exists(cont)

			case _ ⇒ false //Digit
		}

	}


	def diff(dx: String)(arg: MathTerm): MathTerm = {
		Difference.dif(dx,Difference.^#)(arg)
	}

	def diff(arg:MathTerm) :MathTerm = {
		Difference.difOne(Difference.^#)(arg)
	}

	def diffStep(arg: MathTerm): MathTerm = {
		Difference.difOne(Difference.^@)(arg)
	}

	/**
	  * Определить значения функциям
	  */
	def setupDif(): Unit = {
		setupPowDiff()
	}

	/**
	  * Определить значения функциям
	  */
	def setupPowDiff(): Unit ={
		for (op ← func) {
			op.dif = op match {
				case `sin` ⇒ (f) ⇒ (cos, f)
				case `cos` ⇒ (f) ⇒ №(neg, (sin, f))
				case `tg` ⇒ (f) ⇒ (pow_1, №(sqr, (cos, f)))
				case `ctg` ⇒ (f) ⇒ /(-1, №(sqr, (sin, f)))
				case `ln` ⇒ (f) ⇒ /(f)
				case `lg` ⇒ (f) ⇒ /( №(f , mul , (ln, C10)))
				case `sqr` ⇒ (f) ⇒ (2, mul, f) //x*x
				case `pow_1` ⇒ (f)	⇒ /(-1, №(sqr,f))
				case `sqrt` ⇒ (f) ⇒ /( №(2, mul, (sqrt, f)))
				case `neg` ⇒ (f) ⇒ C_1 // -1*x
				case `abs` ⇒ (f) ⇒ (sgn, f)
				case `sgn` ⇒ _ ⇒ C0

				case `exp` ⇒ (f) ⇒ (exp, f)
				case `deg` ⇒ (f) ⇒ /(180, pi)
				case `rad` ⇒ (f) ⇒ /(pi, C180)

				case `arcsin` ⇒ (f) ⇒ /( №(sqrt, №(1, sub, (sqr, f))))
				case `arccos` ⇒ (f) ⇒ /(-1, №(sqrt, №(1, sub, (sqr, f))))
				case `arctg` ⇒ (f) ⇒ /( №(1, add, (sqr, f)))
				case `arcctg` ⇒ (f) ⇒ /(-1, №(1, add, (sqr, f)))

				case `sh` ⇒ (f) ⇒ (ch, f)
				case `ch` ⇒ (f) ⇒ (sh, f)
				case `th` ⇒ (f) ⇒ /( №(sqr, (ch, f)))
				case `cth` ⇒ (f) ⇒ /(-1, №(sqr, (sh, f)))

				case `arsh` ⇒ (f) ⇒ /( №(sqrt, (№(sqr, f), add, 1)))
				case `arch` ⇒ (f) ⇒ /( №(sqrt, (№(sqr, f), sub, 1)))
				case `arth` ⇒ (f) ⇒ /( №(1, sub, (sqr, f)))
				case `arcth` ⇒ (f) ⇒ /( №(1, sub, (sqr, f)))

				case _ ⇒ (f) ⇒ f
			}
		}

		for (op ← opers) {
			op.dif = op match {
				case `add` ⇒ (df, dg, f, g) ⇒ (df, add, dg)
				case `sub` ⇒ (df, dg, f, g) ⇒ (df, sub, dg)
				case `mul` ⇒ (df, dg, f, g) ⇒ (*(df, g), add, *(f, dg))
				case `div` ⇒ (df, dg, f, g) ⇒
					val pg = g
					/(№(*(df, pg), sub, *(f, dg)), (sqr, pg))
				case `pow` ⇒ (df, dg, f, g) ⇒
					val pf = f
					val pg = g
					(**(pf, pg), mul, №(*(dg, (ln, pf)), add, /(*(pg, df), pf)))

				case `log` ⇒ (df, dg, f, g) ⇒
					val pf = f
					val pg = g
					/(№((№(ln, pf), mul, /(dg, pg)), sub, №(№(ln, pg), mul, /(df, pf))),  №(sqr, (ln, pf)))
				case _ ⇒ (df, dg, f, g) ⇒ CalcException.undefinedOperator(s"$op [$f][$g]")
			}

			op.difLeft = op match {
				case `add` ⇒ (df, f, g) ⇒ df
				case `sub` ⇒ (df, f, g) ⇒ df
				case `mul` ⇒ (df, f, g) ⇒ ^^(df, g)
				case `div` ⇒ (df, f, g) ⇒ ^^(df, /( g))
				case `pow` ⇒ (df, f, g) ⇒
					val pg = g
					^^(df, *(**(f, --(pg, C1)), pg))
				case `log` ⇒ (df, f, g) ⇒
					val pf = f
					^^(df, /((neg, №(ln, g)), №(№(sqr, (ln, pf)), mul, pf)))
				case _ ⇒ (df, f, g) ⇒ CalcException.undefinedOperator(s"$op [$f][$g]")
			}

			op.difRight = op match {
				case `add` ⇒ (dg, f, g) ⇒ dg
				case `sub` ⇒ (dg, f, g) ⇒ ---(dg)
				case `mul` ⇒ (dg, f, g) ⇒ ^^(dg, f)
				case `div` ⇒ (dg, f, g) ⇒ ^^(dg, /(---(f), (sqr, g)))
				case `pow` ⇒ (dg, f, g) ⇒ ^^(dg, №(**(f, g), mul, (ln, f)))
				case `log` ⇒ (dg, f, g) ⇒ ^^(dg, /( *(g, (ln, f))))
				case _ ⇒ (dg, f, g) ⇒ CalcException.undefinedOperator(s"$op [$f][$g]")
			}

		}
	}

	val pi = MathConst("pi")
	val e = MathConst("e")
	val IMPL_1 = MathConst("i")
	val C0 = new Digit0
	val C1 = new Digit1
	val C_1 = new Digit_1

	val C2 = new IntDigit(2)
	val C3 = new IntDigit(3)
	val C4 = new IntDigit(4)
	val C6 = new IntDigit(6)
	val C10 = new IntDigit(10)
	val C12 = new IntDigit(12)
	val C180 = new IntDigit(180)






	val add = new CommunicateOperator("+",4)
	val sub = new Operator("−",4)

	val mul = new CommunicateOperator("∙",5)

	val div = new Operator("÷",5)
	val mod = new Operator("mod",5)

	val log = new Operator("log",7)

	val pow = new Operator("^",6)

	val less = new Operator("<",1)
	val more = new Operator(">",1)
	val equa = new Operator("=",0)
	//===========================
	val deg = new Func1("deg")
	val rad = new Func1("rad")
	val exp = new Func1("exp")

	val sqr = new PowFunc1("^2",C2)
	val pow_1 = new PowFunc1("^-1",C_1)

	val C1_2 = /(1,C2)


	val sqrt = new PowFunc1(" √",C1_2)

	val neg = new Func1("-")

	val ln = new Func1("ln")
	val lg = new Func1("lg")

	val abs = new Func1("abs")

	val sgn = new Func1("sgn")
	//	val pow0 = new Func1("^0")
	val sin = new Func1("sin")

	val cos = new Func1("cos")

	val tg = new Func1("tg")
	val ctg = new Func1("ctg")

	//======================
	val arcsin = new Func1("arcsin")
	val arccos = new Func1("arccos")
	val arctg = new Func1("arctg")
	val arcctg = new Func1("arcctg")
	//==================
	val sh = new Func1("sh")
	val ch = new Func1("ch")
	val th = new Func1("th")
	val cth = new Func1("cth")
	//======================
	val arsh = new Func1("arsh")
	val arch = new Func1("arch")
	val arth = new Func1("arth")
	val arcth = new Func1("arcth")

	//======================
	val empty = new Operator(" ",0)


	val pi_2 = /(pi, C2)
	val pi_3_2 = /(MathTerm3(C3, mul, pi), C2)
	val pi_3 = /(pi, C3)
	val pi_4 = /(pi, C4)
	val pi_6 = /(pi, C6)
	val pi_12 = /(pi, C12)
	val CSQRT_2 = MathTerm2(sqrt,C2)
	val CSQRT_3 = MathTerm2(sqrt,C3)

	//=========================

	val func = Seq(sin, cos, tg, ctg,
		ln, lg,sqr, sqrt, pow_1,
		neg, abs, sgn,
		exp, deg, rad,
		arcsin, arccos, arctg, arcctg,
		sh, ch, th, cth,
		arsh, arch, arth, arcth)

	val opers = Seq(add, sub, mul, div, pow, log, empty)


	def isZero(value: Num) = 0 == value

	def print(text: String) = println(text)

	//	def simple(r: Any) = {
	//		var x = r
	//
	//		x = PowerSimlipficator.pronesenie(x)
	//		x = ZvenoSimplificator.sort(x)
	//
	//		Siplificator.power(x)
	//	}
	//====================
	setupDif()

	//setupCalc()
	//	def outText(d: Any): String = {
	//		OutExpression(d) //.toString().replace(',', ' ')
	//	}
}


class Operator(val name: String,val level:Int) {

	var dif: (⇒ MathTerm, ⇒ MathTerm, ⇒ MathTerm, ⇒ MathTerm) ⇒ MathTerm = { (_, _, _, _) => CalcException.differenceNotSupport(name) }
	var difLeft: (⇒ MathTerm, ⇒ MathTerm, ⇒ MathTerm) ⇒ MathTerm = { (_, _, _) => CalcException.differenceNotSupport(name)  }
	var difRight: (⇒ MathTerm, ⇒ MathTerm, ⇒ MathTerm) ⇒ MathTerm = { (_, _, _) => CalcException.differenceNotSupport(name)  }

	override def toString: String = name

	override def equals(other: Any): Boolean = {
		other match {
			case x: Operator ⇒ toString == x.toString
			case _ ⇒ false
		}
	}

}



