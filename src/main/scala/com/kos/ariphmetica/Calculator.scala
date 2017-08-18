package com.kos.ariphmetica

import com.kos.ariphmetica.math.Operator
import com.kos.ariphmetica.math.algorithms.{Calculate, CopositeFunction}
import com.kos.ariphmetica.math.terms.MathTerm
import com.kos.ariphmetica.parser.{ArithParser, PowArithParser, StandardArithParser}

import scala.util.{Failure, Success}

/**
  * Класс инкапсулирет в себе методы для вычисления функций
  * Created by Kos on 23.03.2017.
  */
object Calculator {

	private[this] var parser:String ⇒ ArithParser = new PowArithParser(_)


	def res(x:String) = parse(x)
	def diff(x:String) = Operator.diff(parse(x))
	def calc(x:String) =  Calculate.simpleCalc(CopositeFunction.composePow(CopositeFunction.compose(diff(x))))
	def calc2(x:String) = Calculate.simpleCalc(CopositeFunction.composePow(CopositeFunction.compose(calc(x))))
	//def calc3(x:String) = Calculate.simpleCalc(CopositeFunction.composePow(CopositeFunction.compose(calc2(x))))

	/**
	  * Удалить пробелы из входной строки
	  * @param expressionSpace строка с пробелами
	  * @return строка без пробелов
	  */
	def removeSpace(expressionSpace: String) = {
		expressionSpace.replaceAll("\\s", "")
	}

	/**
	  * Инициализировать параметры по умолчанию для выичления
	  */
	def init(): Unit = {
		Operator.setupDif()
		parser = new PowArithParser(_)
	}

	/**
	  * Инициализировать параметры классического стиля
	  */
	def initClassic(): Unit = {
		Operator.setupClassicDiff()
		parser = new StandardArithParser(_)
	}

	/**
	  * Распарсить входную строку
	  * @param expression - текстовое выражения
	  * @return вычисляемая функция
	  * @throws Throwable если не удалось распарсить
	  */
	def parse(expression:String):MathTerm = {
		parser(expression).InputLine.run() match{
			case Success(ast) ⇒ ast
			case Failure(y) ⇒ throw y
		}
	}

	/**
	  * Распарсить входную строку
	  * @param expression - текстовое выражения
	  * @return вычисляемая функция
	  * @throws Throwable если не удалось распарсить
	  */
	def parseWithSpace(expression:String):MathTerm = {
		parse(removeSpace(expression))
	}

	/**
	  * Выполнить вычисления, для которых не будет потерь данных
	  * @param f вычисляемая функция
	  * @return новая функция (может даже быть без изменений)
	  */
	def simpleCalc(f:MathTerm):MathTerm ={
		Calculate.simpleCalc(CopositeFunction.composePow(CopositeFunction.compose(f)))
	}

	/**
	  * Вычислить всё что вычисляется
	  * @param f вычисляемая функция
	  * @return новая функция (может даже быть без изменений)
	  */
	def fullCalc(f:MathTerm):MathTerm ={
		Calculate.calc(CopositeFunction.compose(f))
	}

	/**
	  * Проверка содержит ли функция дифференцирование
	  * @param f проверяемая функция
	  * @return true если содержит иначе false
	  */
	def containsDiff(f:MathTerm):Boolean = {
		Operator.containsDiff(f)
	}

	/**
	  * отсортировать элементы коммуникативных операций (+ *)
	  * @param f вычисляемая функция
	  * @return новая функция (может даже быть без изменений)
	  */
	def orderedCommunicative(f:MathTerm):MathTerm ={
		CopositeFunction.compose(f)
	}

	/**
	  * вычислить производную если она есть в выражении
	  * @param f вычисляемая функция
	  * @return функция с вычисленными производными
	  */
	def diff(f:MathTerm):MathTerm = {
		Operator.diff(f)
	}

	/**
	  * Выполнить один шаг вычисления производной
 	  * @param f вычисляемая функция
	  * @return шаг выполнения вычисления
	  */
	def diffStep(f:MathTerm):MathTerm = {
		Operator.diffStep(f)
	}
}
