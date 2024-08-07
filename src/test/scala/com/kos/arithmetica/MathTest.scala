package com.kos.arithmetica

import com.kos.ariphmetica.math.algorithms.IntMath
import org.scalatest._
import flatspec._
import matchers._
/**
  * Created by Kos on 23.03.2017.
  */
class MathTest extends AnyFlatSpec with should.Matchers  {

	"even" should "" in {
		IntMath.isEven(1) should be (false)
		IntMath.isEven(11) should be (false)
		IntMath.isEven(1233477) should be (false)
		IntMath.isEven(-1233477) should be (false)

		IntMath.isEven(0) should be (true)
		IntMath.isEven(2) should be (true)
		IntMath.isEven(8) should be (true)
		IntMath.isEven(32456510) should be (true)
		IntMath.isEven(-32456510) should be (true)
	}

	"power" should " 1" in {
		IntMath.pow(1,1) should be (1)
		IntMath.pow(1,0) should be (1)
		IntMath.pow(1,10) should be (1)
		IntMath.pow(1,21) should be (1)
		IntMath.pow(1,345) should be (1)
		IntMath.pow(1,204) should be (1)
	}

	"power" should " 10" in {
		IntMath.pow(10,1) should be (10)
		IntMath.pow(10,0) should be (1)
		IntMath.pow(10,10) should be (10000000000L)
		IntMath.pow(10,21) should be (10000000000L*10000000000L*10)
		IntMath.pow(10,2) should be (100)
		IntMath.pow(10,4) should be (10000)
		IntMath.pow(10,5) should be (100000)
		IntMath.pow(10,7) should be (10000000L)
	}

	"power" should " 11" in {
		IntMath.pow(11,1) should be (11)
		IntMath.pow(11,0) should be (1)
		IntMath.pow(11,11) should be (285311670611L)
		IntMath.pow(11,12) should be (3138428376721L)
		IntMath.pow(11,2) should be (121)
		IntMath.pow(11,3) should be (1331)
		IntMath.pow(11,5) should be (161051)
		IntMath.pow(11,7) should be (19487171)
	}

	"power" should " 2" in {
		IntMath.pow(2,1) should be (2)
		IntMath.pow(2,0) should be (1)
		IntMath.pow(2,10) should be (1024)
		IntMath.pow(2,20) should be (1024*1024)
		IntMath.pow(2,30) should be (1024L*1024L*1024L)
		IntMath.pow(2,3) should be (8)
		IntMath.pow(2,5) should be (32)
		IntMath.pow(2,7) should be (128)
		IntMath.pow(2,8) should be (256)
	}

	"power" should " 0" in {
		IntMath.pow(0,1) should be (0)
		IntMath.pow(0,0) should be (1)
		IntMath.pow(0,10) should be (0)
		IntMath.pow(0,20) should be (0)
		IntMath.pow(0,30) should be (0)
		IntMath.pow(0,3) should be (0)
		IntMath.pow(0,5) should be (0)
		IntMath.pow(0,7) should be (0)
		IntMath.pow(0,8) should be (0)
	}

	"power" should " 3" in {
		IntMath.pow(3,1) should be (3)
		IntMath.pow(3,0) should be (1)
		IntMath.pow(3,2) should be (9)
		IntMath.pow(3,10) should be (243*243)
		IntMath.pow(3,3) should be (27)
		IntMath.pow(3,5) should be (243)
		IntMath.pow(3,7) should be (243*9)
		IntMath.pow(3,8) should be (243*27)
	}
}
