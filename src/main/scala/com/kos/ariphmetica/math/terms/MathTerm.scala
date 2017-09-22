package com.kos.ariphmetica.math.terms

import com.kos.ariphmetica.math.{Func1, Operator}

import scala.math.{Ordered, Ordering}

/**
  * <p> Вычисляемая единица </p>
  * <p> Created by Kos on 20.03.2017. </p>
  * <p> Существуют следующие: </p>
  * <p> `MathTerm2` - функция </p>
  * <p> `MathTerm3` - оператор </p>
  * <p> `DiffTerm`  - производная </p>
  * <p> `MathConst` - константа, переменная </p>
  * <p> `Digit` 	  - число </p>
  * <p> 	`IntDigit`   - целое число </p>
  * <p> 	`Digit0`   -  0 </p>
  * <p> 	`Digit1`   -  1 </p>
  * <p> 	`Digit_1`	 - -1 </p>
  * <p> 	`FloatDigit` - вещественное число </p>
  * <p> 	`MathDigitError` - неправильное число </p>
  * <p> `ArithFun` - неизвестная функция (возможно с несколькими переменными) !!!Не обрабатывается!!! </p>
  * <p> `MathTermError` - неправильный терм </p>
  */
class MathTerm() extends Ordered[MathTerm]{

	def orderValue=1
	def compareIndex(that:MathTerm): Int = {
		0
	}

	override def compare(that: MathTerm): Int = {
		val a=orderValue.compare(that.orderValue)
		if (a==0){
			compareIndex(that)
		}else
			a
	}
}

case class MathTerm2(func:Func1,arg: MathTerm) extends MathTerm{
	override def toString = s"$func($arg)"

	override def orderValue: Int = 20
	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:MathTerm2 ⇒ func.name.compare(x.func.name)
			case _ ⇒ 0
		}
	}
}

case class MathTerm3(left:MathTerm,operator: Operator,right: MathTerm) extends MathTerm{
	override def toString = {
		s"($left $operator $right)"

	}

	override def orderValue: Int = 21

	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:MathTerm3 ⇒ operator.name.compare(x.operator.name)
			case _ ⇒ 0
		}
	}
}

object MathTerm extends Ordering[MathTerm]{
	def apply(value: (MathTerm, Operator, MathTerm)) = {
		MathTerm3(value._1, value._2, value._3)
	}



//	implicit def orderingToOrdered[MathTerm](x: MathTerm)(implicit ord: Ordering[MathTerm]): Ordered[MathTerm] =
//		new Ordered[MathTerm] { def compare(that: MathTerm): Int = ord.compare(x, that) }
	override def compare(x: MathTerm, y: MathTerm): Int = {
		x.compare(y)
	}
}
