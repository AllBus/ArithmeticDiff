package com.kos.ariphmetica.math.terms

/**
  * Created by Kos on 21.03.2017.
  */
case class DiffTerm(mathTerm: MathTerm,dx:String) extends MathTerm{

	override def toString: String = s"f'$dx($mathTerm)"
	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:DiffTerm ⇒ dx.compare(x.dx)
			case _ ⇒ 0
		}
	}

	override def orderValue: Int = 25
}


case class UndefinedDiff(mathTerm: MathTerm, dx:String) extends MathTerm{

	override def toString: String = s"f'$dx($mathTerm)"
	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:UndefinedDiff ⇒ dx.compare(x.dx)
			case _ ⇒ 0
		}
	}
	override def orderValue: Int = 26
}