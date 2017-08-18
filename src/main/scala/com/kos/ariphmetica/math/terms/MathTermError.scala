package com.kos.ariphmetica.math.terms

/**
  * Created by Kos on 21.03.2017.
  */
case class MathTermError(name:String) extends MathTerm {
	override def orderValue: Int = 3
	override def toString: String = {s"Error: $name"}
}

