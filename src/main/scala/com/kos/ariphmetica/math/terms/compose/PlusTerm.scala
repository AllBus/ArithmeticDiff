package com.kos.ariphmetica.math.terms.compose
import com.kos.ariphmetica.math.{C0, C1}
import com.kos.ariphmetica.math.ConstructorOperator.**
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.terms.{Digit, MathConst, MathTerm, MathTerm3}

case class PlusTerm(addTerms:Seq[MathTerm], subTerms:Seq[MathTerm]) extends ComposeTerm{

	override def orderValue: Int = 9

	override def dif(dx: String, ^! : (MathTerm, String) ⇒ MathTerm) = {
		def ^(a: MathTerm) = {
			a match {
				case _:Digit ⇒ C0
				case MathConst(x) ⇒ if (x == dx) C1 else C0
				case _ ⇒ ^!(a, dx)
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
			addTerms.tail.foldLeft(
				addTerms.head
			)((x, y) ⇒ MathTerm3(x, add, y))
		)((x,y) ⇒ MathTerm3(x,sub,y))
	}

	override def sort = PlusTerm(addTerms.sorted,subTerms.sorted)


	override def forall(predicate: (MathTerm) ⇒ Boolean) = {
		addTerms.forall(predicate) && subTerms.forall(predicate)
	}

	override def exists(predicate: (MathTerm) ⇒ Boolean) = {
		addTerms.exists(predicate) || subTerms.exists(predicate)
	}

	override def toString = fold.toString // "{+"+addTerms.toString()+" -"+subTerms.toString()+" }" // fold.toString
}
