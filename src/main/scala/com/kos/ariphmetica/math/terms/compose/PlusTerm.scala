package com.kos.ariphmetica.math.terms.compose
import com.kos.ariphmetica.math.ConstructorOperator.**
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.algorithms.Difference
import com.kos.ariphmetica.math.terms.{Digit, MathConst, MathTerm, MathTerm3}

case class PlusTerm(addTerms:Seq[MathTerm], subTerms:Seq[MathTerm]) extends ComposeTerm{

	override def orderValue: Int = 9

	override def dif(dx: String, ^! : (MathTerm, String) ⇒ MathTerm) = {

		def ^(a: MathTerm) = {
			a match {
				case _:Digit ⇒ C0
				case MathConst(x) ⇒ if (x == dx) C1 else C0
				case _ ⇒ Difference.dif(dx,^!)(a) // ^!(a, dx)
			}
		}

		PlusTerm(addTerms.map(^).filterNot(C0 == _),subTerms.map(^).filterNot(C0 == _))
	}

	override def flatMap:PlusTerm = {
		val addB=Seq.newBuilder[MathTerm]
		val subB=Seq.newBuilder[MathTerm]

		addTerms.foreach {
			case x: PlusTerm ⇒
				addB++=x.addTerms
				subB++=x.subTerms
			case x ⇒ addB+=x
		}

		subTerms.foreach {
			case x: PlusTerm ⇒
				addB++=x.subTerms
				subB++=x.addTerms
			case x ⇒ subB+=x
		}

		PlusTerm(addB.result(),subB.result())
	}

	override def fold = {

		subTerms.foldLeft(
			if (addTerms.isEmpty) C0 else {
				addTerms.tail.foldLeft(
					addTerms.head
				)((x, y) ⇒ MathTerm3(x, add, y))
			}
		)((x,y) ⇒ MathTerm3(x,sub,y))
	}

	override def sort = PlusTerm(addTerms.sorted,subTerms.sorted)


	override def forall(predicate: (MathTerm) ⇒ Boolean) = {
		addTerms.forall(predicate) && subTerms.forall(predicate)
	}

	override def exists(predicate: (MathTerm) ⇒ Boolean) = {
		addTerms.exists(predicate) || subTerms.exists(predicate)
	}


	def map(cont: (MathTerm) ⇒ MathTerm):MathTerm={
		PlusTerm(
			addTerms.map(cont),
			subTerms.map(cont)
		)
	}

	def termsString:String  =
		(if (addTerms.nonEmpty) "+{"+addTerms.mkString(" ")+"}" else "") +
		(if (subTerms.nonEmpty)	"-{"+subTerms.mkString(" ")+"}" else "")




	override def toString = termsString// fold.toString
}
