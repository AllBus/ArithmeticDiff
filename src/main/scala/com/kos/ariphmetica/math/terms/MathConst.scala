package com.kos.ariphmetica.math.terms

/**
  * Created by Kos on 20.03.2017.
  */
case class MathConst(name:String)extends MathTerm{
	override def orderValue: Int = 19
	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:MathConst ⇒ name.compare(x.name)
			case _ ⇒ 0
		}
	}

	override def toString: String = { s"$name"}
}
