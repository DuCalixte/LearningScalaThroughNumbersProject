object nextPrime {
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def IsPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0)
  
  def NextPrime(current:Int):Int = {
    if (current <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
    def Iterate(current:Int):Int = IsPrime(current) match {
      case true => return current case false => Iterate(current + 1)
    }
    Iterate(current)
  }

}