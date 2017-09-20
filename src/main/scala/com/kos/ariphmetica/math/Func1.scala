package com.kos.ariphmetica.math

import com.kos.ariphmetica.math.terms.{MathTerm, MathTerm2}
/**
  * Created by Kos on 20.03.2017.
  */
class Func1(val name: String) {
	var dif: (MathTerm) ⇒ MathTerm = (f) ⇒ MathTerm2(new UnknownDiff(this), f)

	var simpleCalc: MathTerm ⇒ MathTerm = (f) ⇒ MathTerm2(this, f)
	var calc: MathTerm ⇒ MathTerm = (f) ⇒ MathTerm2(this, f)

	override def toString: String = name

	override def equals(other: Any): Boolean = {
		other match {
			case x: Func1 ⇒ toString == x.toString
			case _ ⇒ false
		}
	}


}

class UnknownDiff(val func: Func1,val count:Int=1) extends Func1("'"){
	override def toString: String = func.toString+name*count

	override def equals(other: Any): Boolean = {
		other match {
			case x: UnknownDiff ⇒ toString == x.toString
			case _ ⇒ false
		}
	}

}
