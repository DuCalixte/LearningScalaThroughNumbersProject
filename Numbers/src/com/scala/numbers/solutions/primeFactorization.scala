package com.scala.numbers.solutions

object primeFactorization {
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def isPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0)
  
  /* First crack at scala prime factorization */
  def primeFactorization(number:Int):List[(Int,Int)] = isPrime(number) match {
  	case true =>{
  		List((number,1))
  	}
  	case false =>{
  		def deconstruct(n:Int, prime:Int):List[(Int,Int)]={
  			def loop(acc:Int, n:Int, prime:Int):Int ={
  				if (n % prime == 0) loop(acc + 1, n / prime, prime)
  				else acc
  			}
  			loop(0, n, prime) match{
  				case 0=>List()
  				case x=>List((prime, x))
  			}
  		}
  		(2 to number / 2).filter(x => isPrime(x)).map(x => deconstruct(number, x)).filter(y => y != Nil).flatten.toList
  	}
  }

}