object nthPrime {
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def IsPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0)
                                                  //> IsPrime: (number: Int)Boolean
  
  /* Most efficient Nth prime */
  /* Similarly efficient Nth prime method */
  def NthPrime(number:Int) :Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(acc:Int,current:Int):Int = IsPrime(current) match{
  		case true=> {if (number == acc + 1) current else Iterate(acc + 1, current + 1)   }
  		case false=>Iterate(acc, current + 1)
  	}
  	Iterate(0,2)
  }                                               //> NthPrime: (number: Int)Int
  
  /* Similarly efficient Nth prime method */
  def AltNthPrime(number:Int):Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(current:Int, acc:Int, number:Int):Int = IsPrime(current) match{
  		case true => acc == number match{ case true=>current case false=> Iterate(current + 1, acc + 1, number)}
  		case false=>Iterate(current + 1, acc, number)
  	}
  	Iterate(2,1,number)
  }                                               //> AltNthPrime: (number: Int)Int
  
  /* A generic Nth prime method */
  def GenericNthPrime(number:Int): Int =  {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(acc:Int, current:Int):Int = {
  		var cummulator = acc
  		if (IsPrime(current)) cummulator = cummulator + 1
  		if (cummulator == number) return current
  		else Iterate(cummulator, current + 1)
  	}
  	return Iterate(0,2)
  }

}
