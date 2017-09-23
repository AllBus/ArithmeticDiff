package com.kos.ariphmetica.math.algorithms.classic

import com.kos.ariphmetica.math.Operator.{mul, _}
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.{C1, terms, _}

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
}
