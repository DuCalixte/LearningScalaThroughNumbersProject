object isPrime {
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def IsPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0)

}