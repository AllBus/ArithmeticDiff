package com.kos.ariphmetica

import com.kos.ariphmetica.math.Operator
import com.kos.ariphmetica.math.algorithms.{Calculate, CopositeFunction, OutExpression}
import com.kos.ariphmetica.math.terms.MathTerm
import com.kos.ariphmetica.parser.{ArithParser, PowArithParser, StandardArithParser}

import scala.io.StdIn
import scala.util.{Failure, Success}

/**
  * Created by Kos on 19.03.2017.
  */
object Main {

	def main(args: Array[String]): Unit = {
		//example
		Calculator.init()

		var s=""
		while ({
			println("Введите выражение:")
			s=StdIn.readLine()
			s!="*"}){
			try {
				var dif = Calculator.parseWithSpace(s) //Распарсить строку с пробелами и получаем выражение которое можно вычислить
				println(s"Вычисляем: " + OutExpression(dif))
				dif = CopositeFunction.compose(dif)
				println(s"0) " + OutExpression(dif))
				var i = 0
				do {
					i += 1
					dif = Calculator.diffStep(dif)  // Вычисление одного шага дифференцирования
					println(s"$i} " + OutExpression(dif))
					dif = Calculate.simpleCalc(CopositeFunction.compose(dif)) //Calculator.simpleCalc(dif) // CopositeFunction.compose(dif) //
//					dif =CopositeFunction.compose(dif)
//					println(s"$i) " + OutExpression(dif))
//
//					dif = Calculator.diffStep(dif)  // Вычисление одного шага дифференцирования
					println(s"$i} " + OutExpression(dif))

				} while (Calculator.containsDiff(dif) && i < 100)

				val out = Calculator.fullCalc(dif)
				println("Ответ:  " + OutExpression(out))
			}catch {
				case _: Throwable =>
					println("В выражении ошибка")
			}
		}
	}

	def removeSpace(expressionSpace: String) = {
		expressionSpace.replaceAll("\\s", "")
	}

	def parse(expression: String):MathTerm = {
		new PowArithParser(expression).InputLine.run() match{
		case Success(ast) ⇒ ast
		case Failure(y) ⇒ throw y
		}
	}




}

