package com.kos.ariphmetica.math.algorithms

import com.kos.ariphmetica.math.{CommunicateOperator, PowFunc1}
import com.kos.ariphmetica.math.Operator.{mul, _}
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.terms.compose.{ComposeTerm, MulTerm, PlusTerm}

import scala.annotation.tailrec

/**
  * Created by Kos on 22.03.2017.
  */
object CopositeFunction {

	import com.kos.ariphmetica.math.ConstructorOperator._

	def replaceDivToPow(term: MathTerm): MathTerm = {

		def ^(x: MathTerm): MathTerm = {
			replaceDivToPow(x)
		}

		term match {
			case MathTerm3(C1, `div`, y) ⇒ MathTerm2(pow_1, ^(y))
			case MathTerm3(x, `div`, y) ⇒ MathTerm3(^(x), mul, MathTerm2(pow_1, ^(y)))
			case MathTerm3(x, op, y) ⇒ MathTerm3(^(x), op, ^(y))
			case MathTerm2(op, y) ⇒ MathTerm2(op, ^(y))
			case DiffTerm(x, dx) ⇒ DiffTerm(^(x), dx)
			case x ⇒ x
		}

	}

	def replacePowToDiv(term: MathTerm): MathTerm = {

		def ^(x: MathTerm): MathTerm = {
			replacePowToDiv(x)
		}

		term match {
			case MathTerm3(x, `mul`, MathTerm2(`pow_1`, y)) ⇒ MathTerm3(^(x), `div`, ^(y))
			case MathTerm3(x, op, y) ⇒ MathTerm3(^(x), op, ^(y))
			case MathTerm2(`pow_1`, y) ⇒ MathTerm3(C1, div, ^(y))
			case MathTerm2(op, y) ⇒ MathTerm2(op, ^(y))
			case DiffTerm(x, dx) ⇒ DiffTerm(^(x), dx)
			case x ⇒ x
		}

	}


	//implicit val mathOrdering = new Ordering[MathTerm]()

//
//	def composeMul(terms: Seq[MathTerm]): MathTerm = {
//		//todo: надо проверять наличие abs
//		var b = terms.map(extractPower).sorted
//
//		//	println(b)
//		var (tx, tn) = b.head
//		var r = Seq.newBuilder[(MathTerm, MathTerm)]
//		b = b.tail
//		while (b.nonEmpty) {
//			val (hx, hn) = b.head
//			if (tx == hx) {
//				tn = MathTerm3(tn, `add`, hn)
//			} else {
//				r += tx → tn //**(tx,tn)
//				tx = hx
//				tn = hn
//			}
//			b = b.tail
//		}
//
//
//		r += tx -> tn //**(tx,tn)
//
//		//val a=r.result().sorted
//		MulTerm(r.result())
//		//a.tail.foldLeft(a.head)(MathTerm3(_, mul, _))
//
//	}

	//	def composeMul(head: MathTerm, tail: Seq[MathTerm]): MathTerm = {
	//
	//		val (hx,hv) = extractPower(head)
	//
	//
	//	}




//	def communicative(op: CommunicateOperator, a: MathTerm): Seq[MathTerm] = {
//		a match {
//			case MathTerm3(x, `op`, y) ⇒ communicative(op, x) ++ communicative(op, y)
//			case x ⇒ Seq(x)
//		}
//	}
//
//	def communicative(op: CommunicateOperator, termsComps: Seq[MathTerm]): MathTerm = {
//
//		val terms = termsComps.map(^>)
//
//		op match {
//			case `add` ⇒
//				val a = terms.sorted(MathTerm)
//				a.tail.foldLeft(a.head)(MathTerm3(_, op, _))
//			case `mul` ⇒ composeMul(terms)
//			case _ ⇒
//				val a = terms.sorted(MathTerm)
//				a.tail.foldLeft(a.head)(MathTerm3(_, op, _))
//		}
//	}

	def ^>(x: MathTerm) = compose(x)

	def compose(arg: MathTerm): MathTerm = {

		arg match {
			case MathTerm3(x, `mul`, y) ⇒ communicativeMul(extractMul(x) ++ extractMul(y))
			case MathTerm3(x, `add`, y) ⇒ communicativeAdd(arg)
			case MathTerm3(x, `sub`, y) ⇒ communicativeAdd(arg)
			case x: MulTerm ⇒ communicativeMul(mulTermFlatten(x.terms))
			case x: PlusTerm ⇒ communicativeAdd(x)
			//	case MathTerm3(x, op: CommunicateOperator, y) ⇒ communicative(op, communicative(op, x) ++ communicative(op, y))
			case MathTerm3(x, `pow`, y) ⇒ communicativeMul( mulTermFlat(x , y))// MathTerm3(^>(x), pow, ^>(y))
			case MathTerm3(x, op, y) ⇒ MathTerm3(^>(x), op, ^>(y))

			case MathTerm2(f:PowFunc1, x) ⇒ communicativeMul(mulTermFlat(x ,f.powValue))//MathTerm2(f, ^>(x))
			case MathTerm2(f, x) ⇒ MathTerm2(f, ^>(x))
			case DiffTerm(f, dx: String) ⇒ DiffTerm(^>(f), dx)

			case x ⇒ x
		}
	}



	def communicativeAdd(arg: MathTerm): MathTerm = {
		val terms=extractAdd(arg)

		var plus=terms.addTerms
		var minus=terms.subTerms

		val resadd=Seq.newBuilder[MathTerm]
		val ressub=Seq.newBuilder[MathTerm]

		while (plus.nonEmpty && minus.nonEmpty){
			val ph=plus.head
			val mh=minus.head
			if (ph< mh){
				resadd+=ph
				plus=plus.tail
			}else
			if (mh < ph){
				ressub+=mh
				minus=minus.tail
			}else
			if (ph == mh) {
				minus=minus.tail
				plus=plus.tail
			}else{
				resadd+=ph
				plus=plus.tail
			}
		}

		resadd++=plus
		ressub++=minus

		PlusTerm(unionAdd(resadd.result()),unionAdd(ressub.result()))
	}

	def unionAdd(terms: Seq[MathTerm]):Seq[MathTerm]={
		if (terms.isEmpty)
			terms
		else {
			var r = Seq.newBuilder[MathTerm]

			var b = terms
			var tx = b.head
			var tn = 1
			b = b.tail
			while (b.nonEmpty) {
				val hx = b.head
				if (tx == hx) {
					tn += 1
				} else {
					r += *(tn, tx)
					tx = hx
					tn = 1
				}
				b = b.tail
			}

			r += *(tn, tx)
			r.result()
		}
	}

	def communicativeMul(terms: Seq[(MathTerm, MathTerm)]): MathTerm = {

		var b = terms.sortBy(_._1)

		//println(b)//todo: log out
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

		val res=r.result()
		if (res.size == 1)
			**(res.head)
		else {
			//val a=r.result().sorted
			MulTerm(res)
		}
	}

	def extractAdd(arg: MathTerm): PlusTerm = {

		var plus: List[MathTerm] = Nil
		var minus: List[MathTerm] = Nil

		def loop(term:MathTerm,op:Boolean):Unit = {
			term match {
				case x:PlusTerm ⇒
					x.addTerms.foreach(loop(_,op))
					x.subTerms.foreach(loop(_,!op) )
				case MathTerm3(x, `add`, y) ⇒
					loop(x, op)
					loop(y, op)
				case MathTerm3(x, `sub`, y) ⇒
					loop(x, op)
					loop(y, !op)
				case x ⇒
					if (op) plus ::= ^>(x) else minus ::= ^>(x)
//					Seq(
//					if (op)
//						PlusTerm(Seq(x),Nil)
//					else
//						PlusTerm(Nil,Seq(x))
//					)
			}
		}

		loop(arg,op = true)
		PlusTerm(plus.sorted,minus.sorted)
	}

	def extractMul(arg: MathTerm): Seq[(MathTerm, MathTerm)] = {
		arg match {
			case x: MulTerm ⇒ mulTermFlatten(x.terms)
			case MathTerm3(x, `mul`, y) ⇒ extractMul(x) ++ extractMul(y)
			case x ⇒ mulTermFlat(x,C1)
		}
	}

	def mulTermFlat(term: MathTerm , m: MathTerm): Seq[(MathTerm, MathTerm)] = term match {
		//todo: надо проверять наличие abs
		//case (x: MulTerm, C1) ⇒ 			mulTermFlatten(x.terms)
		case x: MulTerm ⇒ mulTermFlatten(x.terms.map(aw ⇒ aw._1 → *(m, aw._2)))
		case MathTerm3(x, `mul`, y) ⇒ 	mulTermFlat(x , m) ++ mulTermFlat(y , m)
		case MathTerm2(op:PowFunc1,x) ⇒mulTermFlat(x , *(m, op.powValue))
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
			case MathTerm2(op:PowFunc1,x) ⇒ (x,op.powValue)
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
			case MathTerm2(op:PowFunc1,x) ⇒ mulpow(*(m, op.powValue), x)
			case MathTerm3(x, `pow`, y: Digit) ⇒ mulpow(*(m, y), x)
			case MathTerm3(x, `pow`, y) ⇒ mulpow(*(m, dpow(y)), x)
			case x ⇒ checkPowAbs(composePow(x), m)
		}
	}

	def composePow(arg: MathTerm): MathTerm = {

		def >(x: MathTerm) = composePow(x)

		arg match {
			case MathTerm2(op:PowFunc1,x) ⇒  mulpow(op.powValue, x)
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
			case x:ComposeTerm ⇒ x.map(>)
			case x ⇒ x
		}
	}
}
