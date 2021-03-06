package com.kos.ariphmetica.math.algorithms.classic

import com.kos.ariphmetica.math.Operator.{mul, _}
import com.kos.ariphmetica.math.algorithms.CopositeFunction.checkPowAbs
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.{ terms, _}

import scala.annotation.tailrec

object ClassicCompositeFunction {

	import ConstructorOperator._

	def communicative(op: CommunicateOperator, a: MathTerm): Seq[MathTerm] = {
		a match {
			case MathTerm3(x, `op`, y) ⇒ communicative(op, x) ++ communicative(op, y)
			case x ⇒ Seq(x)
		}
	}

	//implicit val mathOrdering = new Ordering[MathTerm]()


	def composeMul(terms:Seq[MathTerm]):MathTerm ={
		//todo: надо проверять наличие abs
		var b=terms.map(extractPower) //.sorted

		//	println(b)
		var (tx,tn)=b.head
		var r=Seq.newBuilder[MathTerm]
		b=b.tail
		while (b.nonEmpty){
			val (hx,hn)=b.head
			if (tx == hx){
				tn = MathTerm3(tn,`add`,hn)
			}else{
				r+= **(tx,tn)
				tx=hx
				tn=hn
			}
			b=b.tail
		}


		r+= **(tx,tn)

		val a=r.result().sorted
		//MulTerm(r.result())
		a.tail.foldLeft(a.head)(MathTerm3(_, mul, _))

	}

	//	def composeMul(head: MathTerm, tail: Seq[MathTerm]): MathTerm = {
	//
	//		val (hx,hv) = extractPower(head)
	//
	//
	//	}

	private def extractPower(head: MathTerm) = {
		//todo: надо проверять наличие abs
		head match {
			case MathTerm3(x, `pow`, y) ⇒ (x, y)
			case MathTerm2(`pow_1`, x) ⇒ (x, C_1)
			case MathTerm2(`sqr`, x) ⇒ (x, C2)
			case MathTerm2(`sqrt`,x) ⇒ (x,C1_2)
			case x ⇒ (x, C1)
		}
	}

	def communicative(op: CommunicateOperator, termsComps: Seq[MathTerm]): MathTerm = {

		val terms= termsComps.map(^>)

		op match {
			case `add` ⇒
				val a = terms.sorted(MathTerm)
				a.tail.foldLeft(a.head)(MathTerm3(_, op, _))
			case `mul` ⇒ composeMul( terms)
			case _ ⇒
				val a = terms.sorted(MathTerm)
				a.tail.foldLeft(a.head)(MathTerm3(_, op, _))
		}
	}

	def ^>(x: MathTerm) = compose(x)

	def compose(arg: MathTerm): MathTerm = {

		arg match {
			case MathTerm3(x, op: CommunicateOperator, y) ⇒ communicative(op, communicative(op, x) ++ communicative(op, y))
			case MathTerm3(x, op, y) ⇒ MathTerm3(^>(x), op, ^>(y))
			case MathTerm2(f, x) ⇒ MathTerm2(f, ^>(x))
			case DiffTerm(f, dx: String) ⇒ DiffTerm(^>(f), dx)

			case x ⇒ x
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
