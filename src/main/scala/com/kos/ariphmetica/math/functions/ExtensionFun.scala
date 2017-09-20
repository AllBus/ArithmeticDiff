package com.kos.ariphmetica.math.functions

import com.kos.ariphmetica.math.terms._
import com.kos.ariphmetica.math._

import scala.collection.immutable.Seq

/**
  * Created by Kos on 24.03.2017.
  */
object ExtensionFun {
//	def absApply(funName: String, args: Seq[MathTerm]):MathTerm = {
//		if (args.size == 1)
//			apply(funName, MathTerm2(Operator.abs, args.head))
//		else
//			Fun.absApply(funName,args)
//	}
//
//	def apply(funName: String, x:MathTerm):MathTerm ={
//
//		Fun.apply(funName,x)
//	}
//
//	def apply(funName: String, args: Seq[MathTerm]): MathTerm = {
//		if (args.size==1){
//			return apply(funName,args.head)
//		}
//
//		Fun.apply(funName,args)
//	}

	val period = new Func1("period")
	val floor = new Func1("floor")
	val round = new Func1("round")
	val cbrt = new Func1("3√")
	val factorial = new Func1("!")

	val sec = new Func1("sec")
	val cosec = new Func1("cosec")
	val arcsec = new Func1("arcsec")
	val arccosec = new Func1("arccosec")
	val sech = new Func1("sech")
	val csch = new Func1("csch")
	val arsech = new Func1("arsech")
	val arcsch = new Func1("arcsch")

	val bound = new ArithFun("bound",Fun.empty,_:Seq[MathTerm])
	val min = new ArithFun("min",Fun.empty,_:Seq[MathTerm])
	val max = new ArithFun("max",Fun.empty,_:Seq[MathTerm])
	val sum = new ArithFun("sum",Fun.empty,_:Seq[MathTerm])
	val mult = new ArithFun("mult",Fun.empty,_:Seq[MathTerm])

	/** Гипотенуза */
	val hypot = new ArithFun("hypot",Fun.empty,_:Seq[MathTerm])
	/** Среднее значение */
	val avr = new ArithFun("average",Fun.empty,_:Seq[MathTerm])
	/** Многочлен */
	val poly = new ArithFun("poly",Fun.empty,_:Seq[MathTerm])

	/** Мат. ожидание */
	val meanValue= new ArithFun("M[X]",Fun.empty,_:Seq[MathTerm])
	/** Дисперсия */
	val variance= new ArithFun("D[X]",Fun.empty,_:Seq[MathTerm])
	/** Сумма */
	val quantifier_∑ = new ArithFun("∑",Fun.empty,_:Seq[MathTerm])

	/** Произведение */
	val quantifier_∏ = new ArithFun("∏",Fun.empty,_:Seq[MathTerm])

	/** Интеграл */
	val quantifier_∫ = new ArithFun("∫",Fun.empty,_:Seq[MathTerm])


	/** Рекурентное соотношение */
	val recursion = new ArithFun("recursion",Fun.empty,_:Seq[MathTerm])

	/** */
	val fibonacci = new ArithFun("Fib",Fun.empty,_:Seq[MathTerm])
	/** */
	val euler = new ArithFun("Euler",Fun.empty,_:Seq[MathTerm])

	/** */
	val bernoulli = new ArithFun("Bernoulli",Fun.empty,_:Seq[MathTerm])

	val gcd = new ArithFun("GCD",Fun.empty,_:Seq[MathTerm])
	val lcm = new ArithFun("LCM",Fun.empty,_:Seq[MathTerm])
	val CNK = new ArithFun("C",Fun.empty,_:Seq[MathTerm])


	val ariths=Seq(min,max,sum,mult,hypot,avr,poly,
		meanValue,variance,
		quantifier_∏,quantifier_∑,quantifier_∫,recursion,
		fibonacci,euler,bernoulli,
		gcd,lcm,CNK
	)

	val subInt= new SubIntDigit()
	val subFloat= new SubFloatDigit()

	val minOp= {
		x:Seq[MathTerm] ⇒
			if (x.size<=1){
				x
			}else{
				var i:IntDigit = subInt
				var g:FloatDigit = subFloat
				var a=x.filter{
						case d:IntDigit ⇒
							if (d<i) i=d
							false
						case d: FloatDigit ⇒
							if (d<g) g=d
							false
						case _ ⇒ true
				}

				a= if (i ne subInt)	a :+ i	else a

				a=if (g ne subFloat)a :+ g	else a
				a
			}
	}


	val maxOp= {
		x:Seq[MathTerm] ⇒
			if (x.size<=1){
				x
			}else{
				var i:IntDigit = subInt
				var g:FloatDigit = subFloat
				var a=x.filter{
					case d:IntDigit ⇒
						if (d>i) i=d
						false
					case d: FloatDigit ⇒
						if (d>g) g=d
						false
					case _ ⇒ true
				}

				a= if (i ne subInt)	a :+ i	else a

				a=if (g ne subFloat)a :+ g	else a
				a
			}
	}

////	for (func <- ariths){
//		func.simpleCalc= func match {
//			case `min` ⇒ func
//
//			case _ ⇒
//		}
//	}
//
}

