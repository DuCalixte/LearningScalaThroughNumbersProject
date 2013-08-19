object playWithPrimeFactorizations {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
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
  }                                               //> PrimeFactorization: (number: Int)List[(Int, Int)]


/* Testing and benchmark method above */
  
  def TimingBenchmark(f: => List[(Int,Int)]){
  	val start = System.currentTimeMillis
  	val commit = f
  	val end = System.currentTimeMillis
  	println("It took: " + (end - start) + " milliseconds to obtain " + commit)
  	//commit.map(n=>val (x,y) = n print(x + "^" + y + " + "))
  }                                               //> TimingBenchmark: (f: => List[(Int, Int)])Unit
  
  /* The Tests */


	PrimeFactorization(5)                     //> res0: List[(Int, Int)] = List((5,1))
  PrimeFactorization(24)                          //> res1: List[(Int, Int)] = List((2,3), (3,1))
  PrimeFactorization(12545)                       //> res2: List[(Int, Int)] = List((5,1), (13,1), (193,1))
  PrimeFactorization(12549)                       //> res3: List[(Int, Int)] = List((3,1), (47,1), (89,1))
  
  TimingBenchmark(PrimeFactorization(1002541))    //> It took: 69646 milliseconds to obtain List((17,2), (3469,1))-
  
  
}