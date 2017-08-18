package com.kos.ariphmetica.math.algorithms

import scala.annotation.tailrec

/**
  * Created by Kos on 23.03.2017.
  */
object IntMath {

	@inline
	def highestBit(a:Long):Long = 64- java.lang.Long.numberOfLeadingZeros(a)

	@inline
	def highestBitAbs(a:Long):Long = 64 -java.lang.Long.numberOfLeadingZeros(Math.abs(a))

	@inline
	def isEven(a:Long) : Boolean = (a & 1) == 0

	@tailrec
	def pow(m:Long, a: Long, b: Int): Long = {

		if (b == 0) return m
		if (b == 1) return m*a

		if (isEven(b)) pow(m,a * a, b >> 1) //even a=(a^2)^b/2
		else pow(m*a,a * a, b >> 1) //odd  a=a*(a^2)^b/2
	}

	def pow(a:Long,n:Long):Long = pow(1,a,n.toInt)

	def overPow(a:Long,n:Long): Boolean ={
		if (n<0) return true
		if (n>=Int.MaxValue) return true

		val d=highestBitAbs(a)

		if (d*n >= 64) return true

		false
	}
}
