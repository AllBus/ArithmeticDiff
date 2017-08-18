package com.kos.ariphmetica.math.algorithms

import com.kos.ariphmetica.math.terms.{MathTerm, MathTerm2}
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math._
/**
  * Created by Kos on 24.03.2017.
  */
object TrigonometryCalc {

	import ConstructorOperator._

	def calc(term: MathTerm):MathTerm ={

		def ^(x:MathTerm) = calc(term)

		term match {
			case MathTerm2(`sin` , x) ⇒
				x match {
					case C0 ⇒ C0
					case `pi` ⇒	 C0
					case `pi_6` ⇒ C1_2
					case `pi_4` ⇒ /(CSQRT_2)
					case `pi_3` ⇒ /(CSQRT_3,C2)
					case `pi_2` ⇒ C1
					case `pi_3_2` ⇒ C_1
					case _ ⇒ term
				}

			case MathTerm2(`cos` , x) ⇒
				x match {
					case C0 ⇒ C1
					case `pi` ⇒	 C_1
					case `pi_6` ⇒ /(CSQRT_3,C2)
					case `pi_4` ⇒ /(CSQRT_2)
					case `pi_3` ⇒ C1_2
					case `pi_2` ⇒ C0
					case `pi_3_2` ⇒ C0
					case _ ⇒ term
				}

			case MathTerm2(`tg` , x) ⇒
				x match {
					case C0 ⇒ C0
					case `pi` ⇒	 C0
					case `pi_6` ⇒ /(CSQRT_3)
					case `pi_4` ⇒ C1
					case `pi_3` ⇒ CSQRT_3
					case _ ⇒ term
				}

			case MathTerm2(`ctg` , x) ⇒
				x match {
					case `pi_2` ⇒ C0
					case `pi_3_2` ⇒	 C0
					case `pi_6` ⇒ CSQRT_3
					case `pi_4` ⇒ C1
					case `pi_3` ⇒ /(CSQRT_3)
					case _ ⇒ term
				}
			case x ⇒ x
		}
	}
}
