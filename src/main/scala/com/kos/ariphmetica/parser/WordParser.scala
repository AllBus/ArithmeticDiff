package com.kos.ariphmetica.parser

import org.parboiled2._
/**
  * Created by Kos on 20.03.2017.
  */
trait WordParser  {
	self :Parser ⇒

	import WordParser._

	def word = rule { capture(CharPredicate.Alpha ~ CharPredicate.AlphaNum.*) }

	def floatingPointNumber = rule {
		capture(simpleFloat~withExponent.?)
	} // (\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?}

	def withExponent= rule{ anyOf("eE") ~ signum.? ~ number   }
	def simpleFloat = rule{ minus.? ~ number~dotnumber.? }

	def minus = rule { anyOf("-−") }
	def signum = rule{ ch('+') | minus }
	def dotnumber= rule{'.'~number}
	def number = rule{ CharPredicate.Digit.+}


}

object WordParser{
	val CharDigits= CharPredicate.Digit++"."

}