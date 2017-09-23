package com.kos.ariphmetica

import com.kos.ariphmetica.Main.{removeSpace}
import com.kos.ariphmetica.math.algorithms.{Calculate, CopositeFunction, OutExpression}
import com.kos.ariphmetica.math.terms.MathTerm
import com.kos.ariphmetica.parser.PowArithParser

import scala.util.{Failure, Success}

/**
  * Created by Kos on 26.03.2017.
  */
object MainExample {

	def main(args: Array[String]): Unit = {
		example
	}

	val exprs:Seq[String] =
		("sqrt(3*x)*(1+x)" ::
			"sin(x)+cos(x)+tg(x)+ctg(x)+abs(x)" ::
			"0 + 0 +4 * (x* 5*6)+1*1 " ::
			"abs(x/2-10)" ::
			"abs((x-1)^2*(x+1)^3)" ::
			"70-x^5-(x+4)*8" ::
			"z*x*y*(4+(a+v+56+(11*u*x))+n)*(40*8)" ::
			"x*7/(7*x)" ::
			"2*x^0" ::
			"sqrt(sin(2*x))" ::
			"c*x^f-g" ::
			"1/x" ::
			"log(4+5,x)" ::
			"log (x,7)" ::
			"sin(4 * x)" ::
			"tg(4*8)   *(5+10* (3/(x + 4)))" ::
			"log(6*x,7*x)" ::
			"log(6,x+5)/ln(x^4)" ::
			"45*x/(7*x)" ::
			"x^9" ::
			"21^x" ::
			"4*x+9*x*x*12+4+8" ::
			"sin(x)+(60+9*x)^4" ::
			"cos(x*x+69*sin(5+x+7))" ::
			"""sin(-45.201*56)*cos(5)-(-90+30*23.5)/(5+19)-10.4*((50*x+50)-(50*x+50))""" ::
			"ln (4 + 5*x) ^ 3" ::
			"x*y/d*z/k*f*d/R*h*x" ::
			"x+y-d+z-k+f+d-R+h+x" ::
			"x*y/((d*z)/k*f*d/R*h)*x" ::
			"x+y-((d+z)-k+f+d-R+h)+x" ::
			"x^3+3*x^2-72*x+90 = 3*x^2+6 *x -72" ::
			"x^2*arctg(5*x)/2-x/10+1/50*arctg(5*x) = x*arctg(5*x)+ 5*x^2/(2*(1+25*x^2))-1/10+1/(10*(1+25*x^2)) = x*arctg(5*x)" ::
			"e^(1/(2-x)) = e^(1/(2-x))/(2-x)^2" ::
			"sin((x-3)/x)^(4/5) = 12*cos((x-3)/x)/(5*x^2*sin((x-3)/x)^(1/5))" ::
			"ln (x^2+sqrt(x^4+1)) = 1/(x^2+sqrt(x^4+1))*(2*x+4*x^3/(2*sqrt(x^4+1))) = 2*x/sqrt(x^4+1)" ::
			"x^2/(x+2)= (x^2+4*x)/(x+2)^2" ::
			"x^(1/3)+4^x*ctg(x) = 1/(3*x^(2/3))+4^x*ln(4)*ctg(x)-4^x/(sin(x)^2)" ::
			"ln(x)/(2*x^(3/2))-arcsin(x) = (2- 3*ln(x))/(4*x^(5/2))-1/sqrt(1-x^2)" ::
			"ln(x^3+4/x^2+tg(x)) = (3*x^2-8*x^-3+1/cos(x)^2)/(x^3+4/x^2+tg(x))" ::
			"arcsin(ln(cos(x))) = - tg(x)/ sqrt(1 - ln(cos(x))^2)" ::
			"exp(x^2*tg(x)-sin(cos(x^2))) = x*exp(x^2-tg(x)-sin(cos(x^2)))*(2*tg(x)+x/cos(x)^2+2*cos(cos(x^2))*sin(x^2))" ::
			"sin(x)^cos(x)= sin(x)^cos(x)*(-sin(x)*ln(sin(x))+cos(x)*ctg(x))" ::
			"exp(x)*sin(x) = exp(x)*(sin(x)+cos(x))" ::
			"deg(pi*x) +50" ::
			"rad(x)" ::
			"700*rad(34*x)+arcsin(deg(x*5))" ::
			"(y*50*((x+67)*20*x)^8)^3" ::
			"log(sin(x),ch(x))" ::
			"ln(abs(3*x))" ::
			"ln(3*x)" ::
			"abs(5-abs(23*x)-40)" ::
			"abs(-3*x^6)/(16-x)" ::
			"ln(0*x^2)" ::
			"sin(|√x-6|/√(x*(4-x)))" ::
			"cth(4+sin(45))*x" ::
			"sign(4+8)" ::
			"sin(x)^2/sin(x)^2+67" ::
			"th(x^(2+3-5))" ::
			"abs(x/2-10)" ::
			"1/abs(x)*(1/ln(abs(x)))" ::
			"ln(abs(x))*abs(x)" ::
			"(x-6)^7+x" ::
			"log(2*x,5/x)" ::
			"log(40,x+4)" ::
			"log(12- x,90/30)" ::
			"x+3*(4+x)" ::
			"x^x" ::
			"(34-x)^(70-12)" ::
			"34^(20-x)" ::
			"sin(4*x)*cos(x^3)" ::
			"x*log(8,2)" ::
			"x*log(81,3)" ::
			"x*(729^4-67/cos(12))" ::
			"x*arcsin(0)" ::
			"x*arccos(0)" ::
			"x*arctg(0)" ::
			"x*arcctg(0)" ::
			"x*arcsin(1)" ::
			"x*arccos(1)" ::
			"x*arctg(1)" ::
			"x*arcctg(1)" ::
			"x*arcsin(-1)" ::
			"x*arccos(-1)" ::
			"x*arctg(-1)" ::
			"x*arcctg(-1)" ::
			"x*sh(0)" ::
			"x*ch(0)" ::
			"x*th(0)" ::
			"x*cth(0)" ::
			"x*sh(1)" ::
			"x*ch(1)" ::
			"x*th(1)" ::
			"x*cth(1)" ::
			"x*sh(-1)" ::
			"x*ch(-1)" ::
			"x*th(-1)" ::
			"x*cth(-1)" ::
			"x*arsh(0)" ::
			"x*arch(0)" ::
			"x*arth(0)" ::
			"x*arcth(0)" ::
			"x*arsh(1)" ::
			"x*arch(1)" ::
			"x*arth(1)" ::
			"x*arcth(1)" ::
			"x*arsh(-1)" ::
			"x*arch(-1)" ::
			"x*arth(-1)" ::
			"x*arcth(-1)" ::
			"arcsin|sin(x)|"::
			"lg|sin(x)|"::
			"lg(x^2)"::
			Nil).map("(" + _ + ")'x") ++
			Seq(
				"((a+1)*(sin(x)+cos(y)))'y'a",
				"((a+1)*(sin(x)+cos(y)))'a'y",
				"((a+1)*(-sin(y)))'a",
				"(sin(y)*cos(x))'y",
				"(x^2)'x+(x*y^3)'y",
				"(x^5)'x'x'x",
				"(sin(x)+cos(x+y+z)^2-sqrt(2*x^2+5*y^3))'x'y'z",
				"(sin(x)+cos(x+y+z)^2-sqrt(2*x^2+5*y^3))'y'z'x",
				"(sin(x)+cos(x+y+z)^2-sqrt(2*x^2+5*y^3))'z'x'y",
				"(sin(5+9*7-3)*(2+x))^(7-1-1)",
				"(y*x-x^2)'x/(y-(5-3)*x)",
				"((6-3*x)-3*(2-x))/((6-3*x)-3*(2-x))",
				"lg(1000)",
				"mod(100,8)",
				"(sin(x+y)*cos(x))'x'y",
				"((x^2+7)/(6*x-8))'x",
				"((1 ÷ y) ∙ xs)'y'y'y"
			) ++ Seq(
			"mod|x,2|",
			"a|y|",
			"7^2^3^6",
			"3^2^2^2^2",
			"81^2*100*2456^3*11*12*13*78^4",
			"cos(2-2)",
			"(z*sin(pi)+x*cos(pi)+y*tg(pi))*6",
			"z*sin(0)+x*cos(0)+y*tg(0)",
			"sin(pi)",
			"sin(pi/2)",
			"sin(pi/3)",
			"sin(pi/4)",
			"sin(pi/6)",
			"sin(3*pi/2)",

			"tg(pi)",
			"tg(pi/2)",
			"tg(pi/3)",
			"tg(pi/4)",
			"tg(pi/6)",
			"tg(3*pi/2)",

			"cos(pi)",
			"cos(pi/2)",
			"cos(pi/3)",
			"cos(pi/4)",
			"cos(pi/6)",
			"cos(3*pi/2)",

			"ctg(pi)",
			"ctg(pi/2)",
			"ctg(pi/3)",
			"ctg(pi/4)",
			"ctg(pi/6)",
			"ctg(3*pi/2)",
			"(1/x+1/(x*8)+1/sin(x))'x",
			"(1/x)'x",
			"(sin(x)^cos(x))'x",
			"(4*x*7*x)'x",
			"(3*x^2+5*x^(5/3)-4/x^3)'x",
			"(7+x)^(5*8)*sin(x)^3*(x+7)^8",
			"x*x*x*x",
			"X^2*(7*x)*x^(3*7)*6^5*x^y*6^3"
		)

	private def example = {
		for (expressionSpace ← exprs) {

			println(expressionSpace)
			val expression = removeSpace(expressionSpace)
			//"("+removeSpace(expressionSpace)+")'x"
			val parser = new PowArithParser(expression).InputLine.run()
			//println("Complete " + parser)
			parser match {
				case Success(ast) ⇒
					println("?   " + OutExpression(ast))
					var dif: MathTerm = ast // DiffTerm(ast,"x")

					var i = 0
					//	println(s"$i > " + OutExpression(dif))
					do {
						i += 1
						dif = Calculator.diffStep(dif)
						println(s"$i } " + OutExpression(dif))

						dif = Calculator.simpleCalc(dif)
						println(s"$i > " + OutExpression(dif))


					} while (Calculator.containsDiff(dif) && i < 100)

					//	dif= Operator.diffStep("x")(dif)
					//	println("> "+OutExpression(dif))
					//	dif= Operator.diff("x")(dif)

					//(Operator.containsDiffVar("x")(dif))
					//	println(dif)

					dif = CopositeFunction.compose(dif)
					val out = Calculate.calc(dif)
					//	println("!   " + out)
					println("!   " + OutExpression(out))

				case Failure(x) ⇒
					println(x)
			}
			println()


		}
	}
}
