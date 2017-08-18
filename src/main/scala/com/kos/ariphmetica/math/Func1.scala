package com.kos.ariphmetica.math

import com.kos.ariphmetica.math.terms.{MathTerm, MathTerm2}
/**
  * Created by Kos on 20.03.2017.
  */
class Func1(val name: String) {
	var dif: (MathTerm) ⇒ MathTerm = (f) ⇒ f

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
