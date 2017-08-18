package com.kos.ariphmetica.math.functions

import com.kos.ariphmetica.Num
import com.kos.ariphmetica.math.Operator._
import com.kos.ariphmetica.math.terms.{MathTerm, MathTerm2}
import com.kos.ariphmetica.math._

import scala.collection.immutable.Seq

/**
  * Created by Kos on 20.03.2017.
  */
object Fun {
	def absApply(funName: String, args: Seq[MathTerm]):MathTerm = {
		if (args.size == 1)
			Fun.apply(funName, MathTerm2(Operator.abs, args.head))
		else
		if (args.size==2){
			funName match {
				case "log" ⇒ №((Operator.abs, args.head), log, args(1))
				case _ ⇒ CalcException.undefinedFunction(funName)
			}
		}else
			CalcException.undefinedFunction(funName)
	}

	import Operator._

	def apply(funName: String, x:MathTerm):MathTerm ={

		funName(0) match {
			case 'a' ⇒
				funName match {
					case "arcsin" | "asin" ⇒ (arcsin, x)
					case "arccos" | "acos" ⇒ (arccos, x)
					case "arctg" | "atg" | "arctan" | "atan" ⇒ (arctg, x)
					case "arcctg" | "actg" | "arcctan" | "actan" | "acot" ⇒ (arcctg, x)
					case "arsh" ⇒ (arsh, x)
					case "arch" ⇒ (arch, x)
					case "arth" ⇒ (arth, x)
					case "arcth" ⇒ (arcth, x)
					case "abs" ⇒ (abs, x)

					case _ ⇒ new ArithFun(funName, empty, Seq(x))
				}
			case _ ⇒
				funName match {
					case "sin" ⇒ (sin, x)
					case "cos" ⇒ (cos, x)
					case "tg" | "tan" ⇒ (tg, x)
					case "ctg"| "ctan" | "cot" ⇒ (ctg, x)
					case "sh" ⇒ (sh, x)
					case "ch" ⇒ (ch, x)
					case "th" ⇒ (th, x)
					case "cth" ⇒ (cth, x)

					case "ln" ⇒ (ln, x)
					case "lg" ⇒ (lg, x)
					case "sqrt" ⇒ (sqrt, x)

					case "sgn" | "sign" ⇒ (sgn, x)
					case "exp" ⇒ (exp, x)
					case "deg" ⇒ (deg, x)
					case "rad" ⇒ (rad, x)
					case _ ⇒ new ArithFun(funName, empty, Seq(x))
				}
		}
	}

	def apply(funName: String, args: Seq[MathTerm]): MathTerm = {
		if (funName.length() <= 0) {
			return new ArithFun(funName, empty, args)
		} else if (args.size == 1) {
			apply(funName,args.head)
		} else if (args.size == 2) {
			funName match {
				case "log" ⇒ (args(0), log, args(1))
				case "mod" ⇒ (args(0), mod, args(1))
				case _ ⇒ new ArithFun(funName, empty, args)
			}
		} else
			funName match {
				//      case "sin" ⇒ new ArithFun(funName, args, sin)
				//      case "cos" ⇒ new ArithFun(funName, args, cos)
				//      case "tg" ⇒ new ArithFun(funName, args, tan)
				//      case "ctg" ⇒ new ArithFun(funName, args, ctg)
				//      case "sum" ⇒ new ArithFun(funName, args, sum)
				//      case "ln" => new ArithFun(funName, args, ln)
				//case "log" => new ArithFun(funName, args, log)
				case _ ⇒ new ArithFun(funName, empty, args)
			}
	}
	//  val sin = new Funcs(Math.sin)
	//  val cos = new Funcs(Math.cos)
	//  val tan = new Funcs(Math.tan)
	//  val ln  = new Funcs(Math.log)
	//  val ctg = new Funcs(a ⇒ Math.cos(a) / Math.sin(a))

	//val sum = new UFuncs(0, _ + _)

	val empty = new Funcs(x ⇒ x)

}

class ArithFun(val funName: String, op: AnyOperator, val args: Seq[MathTerm]) extends MathTerm{

	override def orderValue: Int = 27

	override def compareIndex(that:MathTerm): Int ={
		that match {
			case x:ArithFun ⇒ funName.compare(x.funName)
			case _ ⇒ 0
		}
	}


	override def toString = s"$funName(${args.mkString(",")})"

	def head = args.head
	def two = (if (args.size >= 2) args(1) else C0)

	override def equals(other: Any): Boolean = {
		other match {
			case x: ArithFun ⇒ toString == x.toString
			case _ ⇒ false
		}
	}
}

class AnyOperator {

}



class Funcs(val op: MathTerm ⇒ MathTerm) extends AnyOperator {

}