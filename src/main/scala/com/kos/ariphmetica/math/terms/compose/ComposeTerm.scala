package com.kos.ariphmetica.math.terms.compose

import com.kos.ariphmetica.math.terms.MathTerm

abstract class ComposeTerm extends MathTerm{
	def dif(dx:String, ^! : (MathTerm, String) ⇒ MathTerm): MathTerm
	def flatMap : MathTerm
	def fold : MathTerm
	def sort : ComposeTerm
	def forall(predicate: MathTerm ⇒ Boolean) : Boolean
	def exists(predicate: MathTerm ⇒ Boolean) : Boolean

	override def orderValue: Int = 7
}
