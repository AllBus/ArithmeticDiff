package com.kos.ariphmetica.math

import com.kos.ariphmetica.Num
import com.kos.ariphmetica.math.terms.{MathDigitError, MathTerm, MathTermError}

/**
  * Ошибки, возникающие при вычислении
  */
object CalcException {
	def divisionByZero(num:Num) = new MathDigitError("Деление на ноль",num)

	def differenceNotSupport(arg:String) = new MathTermError("Неподдерживаемый оператор "+arg)

	def differenceNotSupportElement(arg:MathTerm) = new MathTermError("Неподдерживаемая функция "+arg)

	def undefinedFunction(arg: String) = MathTermError("Неизвестная функция " + arg)

	def undefinedOperator(arg: String) = MathTermError("Неизвестный оператор " + arg)

	def undefinedValue(arg: String) = MathTermError("Неправильная запись числа " + arg)

	def errorExpression() = MathTermError("Неправильная запись выражения")
}

