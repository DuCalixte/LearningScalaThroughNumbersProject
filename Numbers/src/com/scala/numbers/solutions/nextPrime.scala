package com.scala.numbers.solutions

object nextPrime {
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def isPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0)
  
  /* Showing the next prime. However if current is prime it returns it. */
  def nextPrime(current:Int):Int = {
    if (current <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
    def iterate(current:Int):Int = isPrime(current) match {
      case true => return current case false => iterate(current + 1)
    }
    iterate(current)
  }

}