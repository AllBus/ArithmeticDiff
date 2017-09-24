package com.kos.arithmetica

import org.scalatest.{FlatSpec, Matchers}
import com.kos.ariphmetica.Calculator._
import com.kos.ariphmetica.math.terms.compose.{MulTerm, PlusTerm}
import com.kos.ariphmetica.math._
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.terms.{IntDigit, MathConst, MathTerm, MathTerm3}
import ConstructorOperator._
import com.kos.ariphmetica.math.algorithms.CopositeFunction
class ComposeTermsTest extends FlatSpec with Matchers  {
	init()

	def convertSeqToMul(a :Seq[_]): MathTerm ={
		a.map{
			case x:Seq[_] ⇒ convertSeqToMul(x)
			case (x:Seq[_],p:Int) ⇒ MathTerm3(convertSeqToMul(x),pow, MathConst(p.toString))
			case ( x:MathTerm,p :MathTerm ) ⇒ MathTerm3(x ,pow, p)
			case ( x:Int,p :Int ) ⇒ MathTerm3(MathConst(x.toString),pow, MathConst(p.toString))
		}.reduce(*(_,_))
	}

	def convertSeqToMulTerm(a :Seq[_]): Seq[(MathTerm,MathTerm)] ={
		a.map{
			case x:Seq[_] ⇒ ( MulTerm( convertSeqToMulTerm(x)) → C1):(MathTerm,MathTerm)
			//case xp @( x:MathTerm,p :MathTerm ) ⇒ xp:(MathTerm,MathTerm)
			case ( x:Int,p :Int ) ⇒ (MathConst(x.toString) → MathConst(p.toString)):(MathTerm,MathTerm)
		}
	}

	def convertSeqToLineMul(a :Seq[_]): Seq[(MathTerm,MathTerm)] ={
		a.flatMap{
			case x:Seq[_] ⇒ x.flatMap(v ⇒ convertSeqToLineMul(Seq(v)))
			case x:MulTerm ⇒ x.terms.flatMap(v ⇒ convertSeqToLineMul(Seq(v)))
			//case (x:Seq[_],p:Int) ⇒ x.map(v ⇒ convertSeqToLineMul(Seq(v)) , MathConst(p.toString)))
			case ( x:MathTerm,p :MathTerm ) ⇒ Seq[(MathTerm,MathTerm)](x → p)
			case ( x:Int,p :Int ) ⇒ Seq[(MathTerm,MathTerm)](MathConst(x.toString) → MathConst(p.toString))
		}
	}

	val cx= MathConst("x")
	"MulTerm" should "" in {
		val term= MulTerm(Seq(
			C0 → C1
		))

		term.flatMap should equal(term)
		term.forall(x ⇒ Operator.contain("x")(x)) should equal(false)
		term.exists(x ⇒ Operator.contain("x")(x)) should equal(false)
	}




	"MulTerm 2" should "" in {
		val term= MulTerm(Seq(
			cx → C1,
			cx → MulTerm(Seq(C180 → C2)),
			MulTerm(Seq(C3 → C2,C4 → C1)) → C4,
			№(45, sub ,cx ) → C1
		))

		term.flatMap should equal(
			MulTerm(Seq[(MathTerm,MathTerm)](
				cx → C1,
				cx → MulTerm(Seq(C180 → C2)),
				C3 → *(C4,C2),
				C4 → C4,
				№(45, sub ,cx ) → C1
			))
		)
		term.forall(x ⇒ Operator.contain("x")(x)) should equal(false)
		term.exists(x ⇒ Operator.contain("x")(x)) should equal(true)
	}

	"PlusTerm" should "" in {
		val term= new PlusTerm(Seq(
			C0 , C1
		),Nil)

		term.flatMap should equal(term)
		term.fold should equal(MathTerm3(C0, add, C1))
		term.forall(x ⇒ Operator.contain("x")(x)) should equal(false)
		term.exists(x ⇒ Operator.contain("x")(x)) should equal(false)
	}

	"PlusTerm 2" should "" in {
		val term= PlusTerm(Seq(
			cx , C1,
			cx , PlusTerm(Seq(C180 , C2),Seq(IntDigit(90)))),
			Seq(
			PlusTerm(Seq(C3 , C2) ,Seq(C4 , IntDigit(45))) , C4,
			№(45, sub ,cx ) , C1
			)
		)

		term.flatMap should equal(
			PlusTerm(Seq(
				cx , C1,
				cx , C180 , C2,
				C4 , IntDigit(45)),
				Seq(
					IntDigit(90),
					C3 , C2 , C4,
					№(45, sub ,cx ) , C1
				)

			)
		)

		term.forall(x ⇒ Operator.contain("x")(x)) should equal(false)
		term.exists(x ⇒ Operator.contain("x")(x)) should equal(true)
	}

	"multerm Flat"  should "" in {

		val terms= Seq(1 → 2, Seq(2→ 4,204 → 1, 3 → 7, 23 → 11) , 6 → 8)


		val termMul= convertSeqToMul(terms)
		val termMulTerm=MulTerm( convertSeqToMulTerm(terms))
		val res= convertSeqToLineMul(terms)
		CopositeFunction.extractMul(termMul) should equal(res)
		CopositeFunction.extractMul(termMulTerm) should equal(res)
	}

	"multerm Flat 3"  should "" in {

		val terms= Seq(1 → 2, Seq(Seq(2→ 4,204 → 1), 3 → 7, 23 → 11) , 6 → 8)


		val termMul= convertSeqToMul(terms)
		val termMulTerm= MulTerm(convertSeqToMulTerm(terms))
		val res= convertSeqToLineMul(terms)
		CopositeFunction.extractMul(termMul) should equal(res)
		CopositeFunction.extractMul(termMulTerm) should equal(res)

	}


	"PlusNone" should "" in {
		PlusTerm(Nil,Nil).fold should equal(C0)
	}


	"MulNone" should "" in {
		MulTerm(Nil).fold should equal(C1)
	}
}
