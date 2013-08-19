object playWithNthPrime {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(69); 
  println("Welcome to the Scala worksheet");$skip(223); 
  
  /* Finding if a number is prime
  	 It is assumed you know not to ask whether 0  or 1 is primed or any negative numbers.
  	 Don't be evil! */
  def IsPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0);System.out.println("""IsPrime: (number: Int)Boolean""");$skip(441); 
  
  /* Most efficient Nth prime */
  def NthPrime(number:Int):Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(current:Int, acc:Int, number:Int):Int = IsPrime(current) match{
  		case true => acc == number match{ case true=>current case false=> Iterate(current + 1, acc + 1, number)}
  		case false=>Iterate(current + 1, acc, number)
  	}
  	Iterate(2,1,number)
  };System.out.println("""NthPrime: (number: Int)Int""");$skip(407); 
  
  /* Similarly efficient Nth prime method */
  def AltNthPrime(number:Int) :Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(acc:Int,current:Int):Int = IsPrime(current) match{
  		case true=> {if (number == acc + 1) current else Iterate(acc + 1, current + 1)   }
  		case false=>Iterate(acc, current + 1)
  	}
  	Iterate(0,2)
  };System.out.println("""AltNthPrime: (number: Int)Int""");$skip(422); 
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
  };System.out.println("""GenericNthPrime: (number: Int)Int""");$skip(428); 
  
  /* A starting List with primes */
  /* Nth Method starting with a known list of primes */
  
  /* Finally a method that displays a list of primes of N lengh
  */
  
  /* Testing and benchmark method above */
  
  def TimingBenchmark(f: => Int){
  	val start = System.currentTimeMillis
  	val commit = f
  	val end = System.currentTimeMillis
  	println("It took: " + (end - start) + " milliseconds to obtain " + commit)
  };System.out.println("""TimingBenchmark: (f: => Int)Unit""")}
  
  /* The Tests
  
  // NthPrime
  NthPrime(1)
  NthPrime(10)
  NthPrime(100)
  NthPrime(1000)
  NthPrime(5000)
  TimingBenchmark(NthPrime(10000))
  TimingBenchmark(NthPrime(100000))
  
  // NthPrime
  AltNthPrime(1)
  AltNthPrime(10)
  AltNthPrime(100)
  AltNthPrime(1000)
  AltNthPrime(5000)
  TimingBenchmark(AltNthPrime(10000))
  TimingBenchmark(AltNthPrime(100000))
  
  // NthPrime
  GenericNthPrime(1)
  GenericNthPrime(10)
  GenericNthPrime(100)
  GenericNthPrime(1000)
  GenericNthPrime(5000)
  TimingBenchmark(GenericNthPrime(10000))
  TimingBenchmark(GenericNthPrime(100000))
  */
}
