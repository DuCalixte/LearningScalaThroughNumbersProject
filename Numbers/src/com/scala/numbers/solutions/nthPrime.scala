package com.scala.numbers.solutions

object nthPrime {
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def isPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0)
  
  /* Most efficient Nth prime */
  /* Similarly efficient Nth prime method */
  def nthPrime(number:Int) :Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def iterate(acc:Int,current:Int):Int = isPrime(current) match{
  		case true=> {if (number == acc + 1) current else iterate(acc + 1, current + 1)   }
  		case false=>iterate(acc, current + 1)
  	}
  	iterate(0,2)
  }
  
  /* Similarly efficient Nth prime method */
  def altNthPrime(number:Int):Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def iterate(current:Int, acc:Int):Int = isPrime(current) match{
  		case true => acc == number match{ case true=>current case false=> iterate(current + 1, acc + 1)}
  		case false=>iterate(current + 1, acc)
  	}
  	iterate(2,1)
  }
  
  /* A generic Nth prime method */
  def genericNthPrime(number:Int): Int =  {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def iterate(acc:Int, current:Int):Int = {
  		var cummulator = acc
  		if (isPrime(current)) cummulator = cummulator + 1
  		if (cummulator == number) return current
  		else iterate(cummulator, current + 1)
  	}
  	return iterate(0,2)
  }

}
