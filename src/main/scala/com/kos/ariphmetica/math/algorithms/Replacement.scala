package com.kos.ariphmetica.math.algorithms

import com.kos.ariphmetica.math.terms.compose.ComposeTerm
import com.kos.ariphmetica.math.terms.{DiffTerm, MathTerm, MathTerm2, MathTerm3}

object Replacement {
	def replace(expression: MathTerm, value: MathTerm, newValue: MathTerm): MathTerm = {
		
		def ^>(x: MathTerm) = replace(x, value, newValue)
	
		if (expression == value)
			newValue
		else
			expression match {
				case MathTerm3(f, op, g) ⇒ MathTerm3(^>(f), op, ^>(g))
				case MathTerm2(op, f) ⇒ MathTerm2(op, ^>(f))
				case DiffTerm(f, dx) ⇒ DiffTerm(^>(f), dx)
				case x:ComposeTerm ⇒ x.map(^>)
				case x ⇒ x
			}
	}
}
