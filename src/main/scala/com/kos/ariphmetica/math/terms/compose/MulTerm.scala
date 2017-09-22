package com.kos.ariphmetica.math.terms.compose

import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math._
import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math.ConstructorOperator.**

class MulTerm(val terms: Seq[(MathTerm, MathTerm)]) extends ComposeTerm {

	override def orderValue: Int = 8

	override def dif(dx: String, ^! : (MathTerm, String) ⇒ MathTerm):MathTerm= {

		def ^(a: MathTerm) = {
			a match {
			//	case _:Digit ⇒ C0 //Не будет выполнено никогда, так как a содержит dx
				case MathConst(x) ⇒ if (x == dx) C1 else C0
				case _ ⇒ ^!(a, dx)
			}
		}

		var unDif:List[(MathTerm,MathTerm)]=Nil
		var hasDif:List[(MathTerm,(MathTerm,MathTerm))]=Nil
		var difTerm:List[(MathTerm,MathTerm)]=Nil

		terms.foreach{ t ⇒
			val f= t._1
			val g= t._2
			val ca=Operator.contain(dx)(f)
			val cp=Operator.contain(dx)(g)

			if (ca){
				hasDif::=(if (cp){//f(x)^g(x)
					pow.dif(^(f), ^(g), f, g)
				}else{//f(x)^a
					pow.difLeft(^(f), f, g)
				} → t)
				difTerm::=t
			}else{
				if (cp){//a^f(x)
					hasDif::=pow.difRight(^(g), f, g) → t
					difTerm::=t
				}else{//a
					unDif::=t
				}
			}
		}

		if (difTerm.isEmpty){
			return C0
		}

		new MulTerm((new PlusTerm(hasDif.map{ d ⇒
			var a=(d._1,C1)::Nil
			difTerm.foreach{ q ⇒ if (q ne d._2)	a::=q }
			new MulTerm(a)
		},Nil) → C1) :: unDif)

	}

	override def flatMap: MulTerm = {
		new MulTerm(terms.flatMap {
			case (x: MulTerm, C1) ⇒
				x.terms
			case (x: MulTerm, p: MathTerm) ⇒
				x.terms.map(aw ⇒ aw._1 → MathTerm3(p, mul, aw._2))
			case x ⇒ Seq(x)
		})
	}

	override def fold: MathTerm = {
		terms.tail.foldLeft(**(terms.head))((x, y) ⇒ MathTerm3(x, mul, **(y)))
	}

	override def sort = new MulTerm(terms.sortBy(x ⇒ x._1))

	override def forall(predicate: (MathTerm) ⇒ Boolean) = {
		terms.forall(x ⇒ predicate(x._1) && predicate(x._2))
	}

	override def exists(predicate: (MathTerm) ⇒ Boolean) = {
		terms.exists(x ⇒ predicate(x._1) || predicate(x._2))
	}

	override def toString = fold.toString
}