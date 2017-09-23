package com.kos.ariphmetica.math.algorithms

import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.terms.compose.ComposeTerm
import com.kos.ariphmetica.math.{Func1, Operator}

/**
  * Вывод выражения на экран
  */
object OutExpression {

	/**
	  * Вывод выражения на экран
	  */
	def apply(d: MathTerm): String = {

		d match {
			case MathTerm3(f, `pow`, g) => s"${^(f)} $pow ${^(g)}"
			case MathTerm3(f, `log`, g) => s"$log(${apply(f)}, ${apply(g)})"

			case MathTerm3(f, `sub`, g) => s"${addition(d)}"
			case MathTerm3(f, `add`, g) => s"${addition(d)}"
			case MathTerm3(f, `mul`, g) => s"${multiplication(d)}"
			case MathTerm3(f, op: Operator, g) => s"${^(f)} $op ${^(g)}"

			case MathTerm2(`sqr`, f) => s"${^(f)} ^ 2"
			case MathTerm2(`pow_1`, f) => s"1 / ${^/(f)}"


			case MathTerm2(`neg`, f) => s"$neg${^(f)}"
			case MathTerm2(`abs`, f) => s"|${apply(f)}|"
			case MathTerm2(op: Func1, f) => s"$op(${apply(f)})"
			case DiffTerm(mathTerm,dx) ⇒ s"(${apply(mathTerm)})'$dx"
			case x:ComposeTerm ⇒ s"(${x.toString})"
			case _ => d.toString
		}
	}

	private[this] def ^/(f: MathTerm): String = {
		f match {
			case fd: Digit ⇒ s"${^(f)}"
			case fd: MathConst ⇒ s"${^(f)}"
			case _ ⇒ s"(${apply(f)})"
		}
	}
	private[this] def ^(d: MathTerm): String = {

		d match {
			case MathTerm3 (f, `pow`, g) => s"${^(f)} $pow ${^(g)}"
			case MathTerm3 (f, `log`, g) => s"$log(${apply(f)}, ${apply(g)})"

			case MathTerm3(f, `sub`, g) => s"(${addition(d)})"
			case MathTerm3(f, `add`, g) => s"(${addition(d)})"
			case MathTerm3(f, `mul`, g) => s"(${multiplication(d)})"
			case MathTerm3(f, op: Operator, g) => s"(${^(f)} $op ${^(g)})"

			case MathTerm2(`pow_1`, f) => s"1 / ${^/(f)}"
			case MathTerm2(`sqr`, f) => s"${^(f)} ^ 2"
			case MathTerm2(`neg`, f) => s"($neg${^(f)})"
			case MathTerm2(`abs`, f) => s"|${apply(f)}|"
			case MathTerm2(op: Func1, f) => s"$op(${apply(f)})"
			case DiffTerm(mathTerm,dx) ⇒ s"(${apply(mathTerm)})'$dx"
			case x:ComposeTerm ⇒ s"(${x.toString})"
			case _ => d.toString
		}
	}

	private[this] def addition(d: MathTerm): String = {
		d match {
			case MathTerm3(f, `sub`, g) => s"${addition(f)} $sub "+
				(g match {
//					case MathTerm3(fg, `add`, gg) ⇒ s"(${addition(g)})"
//					case MathTerm3(fg, `sub`, gg) ⇒ s"(${addition(g)})"
					case MathTerm3(fg, `mul`, gg) => multiplication(g)
					case _ ⇒ ^(g)
				})
			case MathTerm3(f, `add`, g) => s"${addition(f)} $add ${addition(g)}"
			case MathTerm3(f, `mul`, g) => multiplication(d) //s"${multiplication(f)} $mul ${multiplication(g)}"
			case _ => ^(d)
		}
	}

	private[this] def multiplication(d: MathTerm): String = {
		d match {
			case MathTerm3(f, `mul`, MathTerm2(`pow_1`, g)) =>
				s"${multiplication(f)} $div ${^/(g)}"
			case MathTerm3( MathTerm2(`pow_1`, g), `mul`, f) =>
				s"${multiplication(f)} $div ${^/(g)}"
			case MathTerm3(f, `mul`, g) => s"${multiplication(f)} $mul ${multiplication(g)}"

			case _ => ^(d)
		}
	}
}