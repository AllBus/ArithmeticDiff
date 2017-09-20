package com.kos.ariphmetica.math.algorithms.classic

import com.kos.ariphmetica.math.ConstructorOperator.{*, --, ---, ^^}
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.{C0, C1, C10, C_1, CalcException, pi, №}

object ClassicOperator {


	def containsDiffVar(dx:String)(arg:MathTerm):Boolean ={
		def cont = containsDiffVar(dx) _
		arg match {
			case MathConst(x) ⇒ false
			case MathTerm3(f, _, g) ⇒ cont(f) || cont(g)
			case MathTerm2(_, f) ⇒ cont(f)
			case DiffTerm(_ ,d) if d == dx ⇒ true
			case DiffTerm(_ ,_) ⇒ false
			case _ ⇒ false //Digit
		}
	}

	def containsDiff(arg:MathTerm):Boolean ={
		arg match {
			case MathConst(x) ⇒ false
			case MathTerm3(f, _, g) ⇒ containsDiff(f) || containsDiff(g)
			case MathTerm2(_, f) ⇒ containsDiff(f)
			case DiffTerm(_ ,_) ⇒ true
			case _ ⇒ false //Digit
		}
	}

	/**
	  * Определить значения функциям
	  */
	def setupClassicDiff() {

		for (op ← func) {
			op.dif = op match {
				case `sin` ⇒ (f) ⇒ (cos, f)
				case `cos` ⇒ (f) ⇒ №(neg, (sin, f))
				case `tg` ⇒ (f) ⇒ (1, div, №(sqr, (cos, f)))
				case `ctg` ⇒ (f) ⇒ (-1, div, №(sqr, (sin, f)))
				case `ln` ⇒ (f) ⇒ (1, div, f)
				case `lg` ⇒ (f) ⇒ (1, div, №(f , mul , (ln, C10)))
				case `sqr` ⇒ (f) ⇒ (2, mul, f) //x*x
				case `pow_1` ⇒ (f)	⇒ (-1, div, №(sqr,f))
				case `sqrt` ⇒ (f) ⇒ (1, div, №(2, mul, (sqrt, f)))
				case `neg` ⇒ (f) ⇒ C_1 // -1*x
				case `abs` ⇒ (f) ⇒ (sgn, f)
				case `sgn` ⇒ _ ⇒ C0

				case `exp` ⇒ (f) ⇒ (exp, f)
				case `deg` ⇒ (f) ⇒ (180, div, pi)
				case `rad` ⇒ (f) ⇒ (pi, div, 180)

				case `arcsin` ⇒ (f) ⇒ (1, div, №(sqrt, №(1, sub, (sqr, f))))
				case `arccos` ⇒ (f) ⇒ (-1, div, №(sqrt, №(1, sub, (sqr, f))))
				case `arctg` ⇒ (f) ⇒ (1, div, №(1, add, (sqr, f)))
				case `arcctg` ⇒ (f) ⇒ (-1, div, №(1, add, (sqr, f)))

				case `sh` ⇒ (f) ⇒ (ch, f)
				case `ch` ⇒ (f) ⇒ (sh, f)
				case `th` ⇒ (f) ⇒ (1, div, №(sqr, (ch, f)))
				case `cth` ⇒ (f) ⇒ (-1, div, №(sqr, (sh, f)))

				case `arsh` ⇒ (f) ⇒ (1, div, №(sqrt, (№(sqr, f), add, 1)))
				case `arch` ⇒ (f) ⇒ (1, div, №(sqrt, (№(sqr, f), sub, 1)))
				case `arth` ⇒ (f) ⇒ (1, div, №(1, sub, (sqr, f)))
				case `arcth` ⇒ (f) ⇒ (1, div, №(1, sub, (sqr, f)))

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
					(№(*(df, pg), sub, *(f, dg)), div, №(sqr, pg))
				case `pow` ⇒ (df, dg, f, g) ⇒
					val pf = f
					val pg = g
					(№(pf, pow, pg), mul, №(*(dg, (ln, pf)), add, (*(pg, df), div, pf)))

				case `log` ⇒ (df, dg, f, g) ⇒
					val pf = f
					val pg = g
					(№((№(ln, pf), mul, №(dg, div, pg)), sub, №(№(ln, pg), mul, №(df, div, pf))), div, №(sqr, (ln, pf)))
				case _ ⇒ (df, dg, f, g) ⇒ CalcException.undefinedOperator(s"$op [$f][$g]")
			}

			op.difLeft = op match {
				case `add` ⇒ (df, f, g) ⇒ df
				case `sub` ⇒ (df, f, g) ⇒ df
				case `mul` ⇒ (df, f, g) ⇒ ^^(df, g)
				case `div` ⇒ (df, f, g) ⇒ ^^(df, (1, div, g))
				case `pow` ⇒ (df, f, g) ⇒
					val pg = g
					^^(df, *((f, pow, --(pg, C1)), pg))
				case `log` ⇒ (df, f, g) ⇒
					val pf = f
					^^(df, №((neg, №(ln, g)), div, №(№(sqr, (ln, pf)), mul, pf)))
				case _ ⇒ (df, f, g) ⇒ CalcException.undefinedOperator(s"$op [$f][$g]")
			}

			op.difRight = op match {
				case `add` ⇒ (dg, f, g) ⇒ dg
				case `sub` ⇒ (dg, f, g) ⇒ ---(dg)
				case `mul` ⇒ (dg, f, g) ⇒ ^^(dg, f)
				case `div` ⇒ (dg, f, g) ⇒ ^^(dg, №(---(f), div, (sqr, g)))
				case `pow` ⇒ (dg, f, g) ⇒ ^^(dg, №((f, pow, g), mul, (ln, f)))
				case `log` ⇒ (dg, f, g) ⇒ ^^(dg, (1, div, *(g, (ln, f))))
				case _ ⇒ (dg, f, g) ⇒ CalcException.undefinedOperator(s"$op [$f][$g]")
			}

		}

	}


	//
	//
	//	def setupCalc():Unit = {
	//		for (op ← func) {
	//			op.calc = op match {
	//				case `sin` ⇒ (f) ⇒ (cos, f)
	//				case `cos` ⇒ (f) ⇒ №(neg, (sin, f))
	//				case `tg` ⇒ (f) ⇒ (pow_1, №(sqr, (cos, f)))
	//				case `ctg` ⇒ (f) ⇒ /(-1, №(sqr, (sin, f)))
	//				case `ln` ⇒ (f) ⇒ /(f)
	//				case `lg` ⇒ (f) ⇒ /( №(f , mul , (ln, C10)))
	//				case `sqr` ⇒ (f) ⇒ (2, mul, f) //x*x
	//				case `pow_1` ⇒ (f)	⇒ /(-1, №(sqr,f))
	//				case `sqrt` ⇒ (f) ⇒ /( №(2, mul, (sqrt, f)))
	//				case `neg` ⇒ (f) ⇒ C_1 // -1*x
	//				case `abs` ⇒ (f) ⇒ (sgn, f)
	//				case `sgn` ⇒ _ ⇒ C0
	//
	//				case `exp` ⇒ (f) ⇒ (exp, f)
	//				case `deg` ⇒ (f) ⇒ /(180, pi)
	//				case `rad` ⇒ (f) ⇒ /(pi, C180)
	//
	//				case `arcsin` ⇒ (f) ⇒ /( №(sqrt, №(1, sub, (sqr, f))))
	//				case `arccos` ⇒ (f) ⇒ /(-1, №(sqrt, №(1, sub, (sqr, f))))
	//				case `arctg` ⇒ (f) ⇒ /( №(1, add, (sqr, f)))
	//				case `arcctg` ⇒ (f) ⇒ /(-1, №(1, add, (sqr, f)))
	//
	//				case `sh` ⇒ (f) ⇒ (ch, f)
	//				case `ch` ⇒ (f) ⇒ (sh, f)
	//				case `th` ⇒ (f) ⇒ /( №(sqr, (ch, f)))
	//				case `cth` ⇒ (f) ⇒ /(-1, №(sqr, (sh, f)))
	//
	//				case `arsh` ⇒ (f) ⇒ /( №(sqrt, (№(sqr, f), add, 1)))
	//				case `arch` ⇒ (f) ⇒ /( №(sqrt, (№(sqr, f), sub, 1)))
	//				case `arth` ⇒ (f) ⇒ /( №(1, sub, (sqr, f)))
	//				case `arcth` ⇒ (f) ⇒ /( №(1, sub, (sqr, f)))
	//
	//				case _ ⇒ (f) ⇒ f
	//			}
	//		}
	//}
}
