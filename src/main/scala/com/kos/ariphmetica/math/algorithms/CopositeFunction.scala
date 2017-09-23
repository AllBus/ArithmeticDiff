package com.kos.ariphmetica.math.algorithms

import com.kos.ariphmetica.math.Operator.{mul, _}
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.terms.compose.{MulTerm, PlusTerm}
import com.kos.ariphmetica.math.{C1, terms, _}

import scala.annotation.tailrec

/**
  * Created by Kos on 22.03.2017.
  */
object CopositeFunction {

	import ConstructorOperator._

	def replaceDivToPow(term: MathTerm): MathTerm = {

		def ^(x: MathTerm): MathTerm = {
			replaceDivToPow(x)
		}

		term match {
			case MathTerm3(C1, `div`, y) ⇒ (pow_1, ^(y))
			case MathTerm3(x, `div`, y) ⇒ MathTerm3(^(x), mul, (pow_1, ^(y)))
			case MathTerm3(x, op, y) ⇒ (^(x), op, ^(y))
			case MathTerm2(op, y) ⇒ (op, ^(y))
			case DiffTerm(x, dx) ⇒ DiffTerm(^(x), dx)
			case x ⇒ x
		}

	}

	def replacePowToDiv(term: MathTerm): MathTerm = {

		def ^(x: MathTerm): MathTerm = {
			replacePowToDiv(x)
		}

		term match {
			case MathTerm3(x, `mul`, MathTerm2(`pow_1`, y)) ⇒ (^(x), `div`, ^(y))
			case MathTerm3(x, op, y) ⇒ (^(x), op, ^(y))
			case MathTerm2(`pow_1`, y) ⇒ (C1, div, ^(y))
			case MathTerm2(op, y) ⇒ (op, ^(y))
			case DiffTerm(x, dx) ⇒ DiffTerm(^(x), dx)
			case x ⇒ x
		}

	}


	//implicit val mathOrdering = new Ordering[MathTerm]()


	def composeMul(terms: Seq[MathTerm]): MathTerm = {
		//todo: надо проверять наличие abs
		var b = terms.map(extractPower).sorted

		//	println(b)
		var (tx, tn) = b.head
		var r = Seq.newBuilder[(MathTerm, MathTerm)]
		b = b.tail
		while (b.nonEmpty) {
			val (hx, hn) = b.head
			if (tx == hx) {
				tn = MathTerm3(tn, `add`, hn)
			} else {
				r += tx → tn //**(tx,tn)
				tx = hx
				tn = hn
			}
			b = b.tail
		}


		r += tx -> tn //**(tx,tn)

		//val a=r.result().sorted
		MulTerm(r.result())
		//a.tail.foldLeft(a.head)(MathTerm3(_, mul, _))

	}

	//	def composeMul(head: MathTerm, tail: Seq[MathTerm]): MathTerm = {
	//
	//		val (hx,hv) = extractPower(head)
	//
	//
	//	}




	def communicative(op: CommunicateOperator, a: MathTerm): Seq[MathTerm] = {
		a match {
			case MathTerm3(x, `op`, y) ⇒ communicative(op, x) ++ communicative(op, y)
			case x ⇒ Seq(x)
		}
	}

	def communicative(op: CommunicateOperator, termsComps: Seq[MathTerm]): MathTerm = {

		val terms = termsComps.map(^>)

		op match {
			case `add` ⇒
				val a = terms.sorted(MathTerm)
				a.tail.foldLeft(a.head)(MathTerm3(_, op, _))
			case `mul` ⇒ composeMul(terms)
			case _ ⇒
				val a = terms.sorted(MathTerm)
				a.tail.foldLeft(a.head)(MathTerm3(_, op, _))
		}
	}

	def ^>(x: MathTerm) = compose(x)

	def compose(arg: MathTerm): MathTerm = {

		arg match {
			case MathTerm3(x, `mul`, y) ⇒ communicativeMul(extractMul(x) ++ extractMul(y))
			case MathTerm3(x, `add`, y) ⇒ communicativeAdd(arg)
			case x: MulTerm ⇒ communicativeMul(mulTermFlatten(x.terms))
			case x: PlusTerm ⇒ communicativeAdd(x)
			//	case MathTerm3(x, op: CommunicateOperator, y) ⇒ communicative(op, communicative(op, x) ++ communicative(op, y))
			case MathTerm3(x, op, y) ⇒ MathTerm3(^>(x), op, ^>(y))
			case MathTerm2(f, x) ⇒ MathTerm2(f, ^>(x))
			case DiffTerm(f, dx: String) ⇒ DiffTerm(^>(f), dx)

			case x ⇒ x
		}
	}

	def communicativeAdd(atg: MathTerm): MathTerm = {
		//todo:
		???
	}

	def communicativeMul(terms: Seq[(MathTerm, MathTerm)]): MathTerm = {
		//todo:
		???
	}

	def extractMul(arg: MathTerm): Seq[(MathTerm, MathTerm)] = {
		arg match {
			case x: MulTerm ⇒ mulTermFlatten(x.terms)
			case MathTerm3(x, `mul`, y) ⇒ extractMul(x) ++ extractMul(y)
			case x ⇒ mulTermFlatten(Seq( extractPower(x)))
		}
	}

	def mulTermFlat(term: MathTerm , m: MathTerm): Seq[(MathTerm, MathTerm)] = term match {
		//case (x: MulTerm, C1) ⇒ 			mulTermFlatten(x.terms)
		case x: MulTerm ⇒	mulTermFlatten(x.terms.map(aw ⇒ aw._1 → *(m, aw._2)))
		case MathTerm3(x, `mul`, y) ⇒ 	mulTermFlat(x , m) ++ mulTermFlat(y , m)
		case MathTerm2(`sqrt`, x) ⇒	 	mulTermFlat(x , *(m, C1_2))
		case MathTerm2(`pow_1`, x) ⇒	mulTermFlat(x , *(m, C_1))
		case MathTerm2(`sqr`, x) ⇒ 	 	mulTermFlat(x , *(m, C2))
		case MathTerm3(x, `pow`, y) ⇒  mulTermFlat(x , *(m, y))
		case x ⇒ Seq(^>(x) → ^>(m))
	}

	def mulTermFlatten(terms: Seq[(MathTerm, MathTerm)]): Seq[(MathTerm, MathTerm)] = {
		terms.flatMap { x ⇒	 mulTermFlat(x._1,x._2)	}
	}


	private def extractPower(head: MathTerm) = {
		//todo: надо проверять наличие abs
		head match {
			case MathTerm3(x, `pow`, y) ⇒ (x, y)
			case MathTerm2(`pow_1`, x) ⇒ (x, C_1)
			case MathTerm2(`sqr`, x) ⇒ (x, C2)
			case MathTerm2(`sqrt`, x) ⇒ (x, C1_2)
			case x ⇒ (x, C1)
		}
	}


	/**
	  * Проверить что полученная степень получается для элемента которому нужен модуль
	  *
	  * @param m
	  * @return
	  */
	def checkPowAbs(x: MathTerm, m: MathTerm): MathTerm = {
		x match {
			case MathTerm2(`abs`, y) ⇒
				**(x, m)
			case _ ⇒
				**(x, m)
			//				m match {
			//					case C1 ⇒ x
			//					case C2 ⇒ MathTerm2(sqr,x)
			//					case C1_2 ⇒ MathTerm2(sqrt,x)
			//					case C_1 ⇒ MathTerm2(pow_1,x)
			////					case y:Digit if y.lessZero && !y.overNegative  ⇒
			////						MathTerm2(pow_1,MathTerm3(x,pow, y.negative))
			//					case _ ⇒ MathTerm3(x, pow, m)
			//				}
			//		MathTerm3((abs, x), pow, m)
			//todo: на случай sqrt(x^2) надо добавлять модуль
		}
	}

	def dpow(arg: MathTerm): MathTerm = {
		mulpow(C1, composePow(arg))
	}

	@tailrec
	private[this] def mulpow(m: MathTerm, arg: MathTerm): MathTerm = {

		arg match {
			case MathTerm2(`sqrt`, x) ⇒ mulpow(*(m, C1_2), x)
			case MathTerm2(`pow_1`, x) ⇒ mulpow(*(m, C_1), x)
			case MathTerm2(`sqr`, x) ⇒ mulpow(*(m, C2), x)
			case MathTerm3(x, `pow`, y: Digit) ⇒ mulpow(*(m, y), x)
			case MathTerm3(x, `pow`, y) ⇒ mulpow(*(m, dpow(y)), x)
			case x ⇒ checkPowAbs(composePow(x), m)
		}
	}

	def composePow(arg: MathTerm): MathTerm = {

		def >(x: MathTerm) = composePow(x)

		arg match {
			case MathTerm2(f@`sqrt`, x) ⇒ mulpow(C1_2, x)
			case MathTerm2(f@`pow_1`, x) ⇒ mulpow(C_1, x)
			case MathTerm2(f@`sqr`, x) ⇒ mulpow(C2, x)
			case MathTerm3(x, `pow`, y: Digit) ⇒ mulpow(y, x)
			case MathTerm3(x, `pow`, y) ⇒ mulpow(dpow(y), x)

			case MathTerm3(x, `mul`, y) ⇒
				val l = >(x)
				val r = >(y)
				if (l == r)
					mulpow(C2, l)
				else {
					//todo: надо проверить наличие abs
					MathTerm3(l, mul, r) match {

						case MathTerm3(MathTerm3(lx, `pow`, plx), `mul`, MathTerm3(rx, `pow`, prx)) if lx == rx ⇒
							MathTerm3(lx, `pow`, MathTerm3(plx, add, prx))
						case MathTerm3(MathTerm2(`sqr`, lx), `mul`, MathTerm3(rx, `pow`, prx)) if lx == rx ⇒
							MathTerm3(lx, `pow`, MathTerm3(C2, add, prx))
						case MathTerm3(MathTerm3(lx, `pow`, plx), `mul`, MathTerm2(`sqr`, rx)) if lx == rx ⇒
							MathTerm3(lx, `pow`, MathTerm3(C2, add, plx))
						case MathTerm3(MathTerm2(`sqr`, lx), `mul`, MathTerm2(`sqr`, rx)) if lx == rx ⇒
							MathTerm3(lx, `pow`, C4)
						case v ⇒ v
					}
				}

			case MathTerm3(x, op@`add`, y) ⇒
				val l = >(x)
				val r = >(y)
				if (l == r)
					MathTerm3(C2, mul, l)
				else
					MathTerm3(l, op, r)

			case MathTerm3(x, op@`sub`, y) ⇒
				val l = >(x)
				val r = >(y)
				if (l == r)
					C0
				else
					MathTerm3(l, op, r)

			case MathTerm3(x, op, y) ⇒ MathTerm3(>(x), op, >(y))
			case MathTerm2(f, x) ⇒ MathTerm2(f, >(x))
			case DiffTerm(f, dx: String) ⇒ DiffTerm(>(f), dx)
			case x ⇒ x
		}
	}
}
