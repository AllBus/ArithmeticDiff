package com.kos.ariphmetica.math

/**
  * Created by Kos on 20.03.2017.
  */
class CommunicateOperator(name: String,level:Int) extends Operator(name,level) {

	override def equals(other: Any): Boolean = {
		other match {
			case x: CommunicateOperator ⇒ toString == x.toString
			case _ ⇒ false
		}
	}
}
