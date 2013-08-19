object playWithPrimeFactorizations {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(80); 
  println("Welcome to the Scala worksheet");$skip(223); 
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def IsPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0);System.out.println("""IsPrime: (number: Int)Boolean""");$skip(596); 
  
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
  };System.out.println("""PrimeFactorization: (number: Int)List[(Int, Int)]""");$skip(331); 


/* Testing and benchmark method above */
  
  def TimingBenchmark(f: => List[(Int,Int)]){
  	val start = System.currentTimeMillis
  	val commit = f
  	val end = System.currentTimeMillis
  	println("It took: " + (end - start) + " milliseconds to obtain " + commit)
  	//commit.map(n=>val (x,y) = n print(x + "^" + y + " + "))
  };System.out.println("""TimingBenchmark: (f: => List[(Int, Int)])Unit""")}
  
  /* The Tests


	PrimeFactorization(5)
  PrimeFactorization(24)
  PrimeFactorization(12545)
  PrimeFactorization(12549)
  
  PrimeFactorization(1002541)
  TimingBenchmark(PrimeFactorization(1002541))
  
  */
}
