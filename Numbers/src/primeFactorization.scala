object primeFactorization {
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def IsPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0)
                                                  //> IsPrime: (number: Int)Boolean
  
  /* First crack at scala prime factorization */
  def PrimeFactorization(number:Int):List[(Int,Int)] = IsPrime(number) match {
  	case true =>{
  		List((number,1))
  	}
  	case false =>{
  		def Deconstruct(n:Int, prime:Int):List[(Int,Int)]={
  			def Loop(acc:Int, n:Int, prime:Int):Int ={
  				if (n % prime == 0) Loop(acc + 1, n / prime, prime)
  				else acc
  			}
  			Loop(0, n, prime) match{
  				case 0=>List()
  				case x=>List((prime, x))
  			}
  		}
  		(2 to number / 2).filter(x => IsPrime(x)).map(x => Deconstruct(number, x)).filter(y => y != Nil).flatten.toList
  	}
  }

}