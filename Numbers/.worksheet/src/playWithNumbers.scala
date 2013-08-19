object playWithNumbers {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(68); 
  println("Welcome to the Scala worksheet");$skip(124); 
  
  //IsPrimeGeneric(2)s
  
  def IsPrimeGeneric(number:Int):Boolean= (2 until number)forall(prime => number % prime != 0);System.out.println("""IsPrimeGeneric: (number: Int)Boolean""");$skip(78); 
  
  def IsPrime(number:Int):Boolean= (2 until number)forall(number % _ != 0);System.out.println("""IsPrime: (number: Int)Boolean""");$skip(498); 
  
  //def IsPrimeWithFormat
  
  def GenericNthPrime(number:Int): Int = number match {
  	case 1=> 2
  	case 2=> 3
  	case 3=> 5
  	case _=>{
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(acc:Int, current:Int):Int = {
  		var cummulator = acc
  		if (IsPrimeGeneric(current)) cummulator = cummulator + 1
  		if (cummulator == number) return current
  		else Iterate(cummulator, current + 1)
  	}
  	return Iterate(3,6)
  	}
  };System.out.println("""GenericNthPrime: (number: Int)Int""");$skip(959); 
  
  def OptimedGenericNthPrime(number:Int): Int = number match {
  	case 1=> 2
  	case 2=> 3
  	case _=> {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(acc:Int, index :Int, start:Int = 2):Int = {
  		def IterateStep(iteration:Int, step:Int, currentLevel:Int)(value:Int = 4 * iteration + step):(Int,Int)= IsPrimeGeneric( value) match{
  			case true => (value, currentLevel + 1)
  			case false => (value, currentLevel)
  		}
  		  		
  		val (first,cummulator1 ):(Int,Int) = IterateStep(index, 1, acc)()
  		val (second,cummulator2 ):(Int,Int) = IterateStep(index, 3, cummulator1)()
			
  		if ((cummulator1 + start) == number) return first
  		else if ((cummulator2 + start) == number) return second
  		else if ((cummulator1 + start) > number) return 0
  		else if ((cummulator2 + start) > number) return 0
  		else Iterate(cummulator2, index + 1)
  		}
  	return Iterate(0,1)
  	}
  };System.out.println("""OptimedGenericNthPrime: (number: Int)Int""");$skip(389); 
  
  
  
  def NthPrime(number:Int): Int =  {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(acc:Int, current:Int):Int = {
  		var cummulator = acc
  		if (IsPrime(current)) cummulator = cummulator + 1
  		if (cummulator == number) return current
  		else Iterate(cummulator, current + 1)
  	}
  	return Iterate(0,2)
  };System.out.println("""NthPrime: (number: Int)Int""");$skip(362); 
  
  def NewNthPrime(number:Int) :Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(acc:Int,current:Int):Int = IsPrime(current) match{
  		case true=> {if (number == acc + 1) current else Iterate(acc + 1, current + 1)   }
  		case false=>Iterate(acc, current + 1)
  	}
  	Iterate(0,2)
  };System.out.println("""NewNthPrime: (number: Int)Int""");$skip(413); 
  
  def FinalNthPrime(number:Int):Int = {
  	if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  	def Iterate(current:Int, acc:Int, number:Int):Int = IsPrime(current) match{
  		case true => acc == number match{ case true=>current case false=> Iterate(current + 1, acc + 1, number)}
  		case false=>Iterate(current + 1, acc, number)
  	}
  	Iterate(2,1,number)
  };System.out.println("""FinalNthPrime: (number: Int)Int""");$skip(791); 
  
  def NthPrimeWithMap(number:Int): Int = number match {
  	case 1=> 2
  	case 2=> 3
  	case _=> {
  		if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  		def Iterate(acc:Int, index :Int, start:Int = 2):Int = {
  			def IteratePrime(ci:Int):List[(Int, Boolean)] = {
  				List(4 * ci + 1, 4 * ci + 3).map( x=> (x, IsPrime(x)))
  			}
  			val (list, state) = IteratePrime(index).unzip
  			val _1 = if (state(0)) acc + 1 else acc
  			val _2 = if (state(1)) _1 + 1 else _1
  			
  			//println("acc = " + _2 + "prime = " + list)
  			if(_1 + start == number) list(0)
  			else if(_2 + start == number) list(1)
  			else if(_1 + start > number || _2 + start > number) 0
  			else Iterate(_2, index + 1)
  			
  		}
  		Iterate(0,1)
  	}
  };System.out.println("""NthPrimeWithMap: (number: Int)Int""");$skip(1019); 
  
  def AlternativeNthPrimeWithMap(number:Int): Int = number match {
  	case 1=> 2
  	case 2=> 3
  	case _=> {
  		if (number <= 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
  		def Iterate(acc:Int, index :Int, start:Int = 2):Int = {
  			def IteratePrime(ci:Int, cl:Int):List[(Int, Int)] = {
  				List(ci).map(x => IsPrime(4 * x + 1) match{
  					case true =>{
  						number == start + cl + 1 match{
  							case true => (4 * x + 1, cl + 1)
  							case false => {
  								IsPrime(4 * x + 3) match{
  									case false => (4 * x + 1, cl + 1)
  									case true =>(4 * x + 3, cl + 2)
  								}
  							}
  						}
  					}
  					case false =>{
  						IsPrime(4 * x + 3) match{
  							case false => (0, cl)
  							case true =>(4 * x + 3, cl + 1)
  						}
  					}
  				})
  			}
  			val (list, level) = (IteratePrime(index, acc)).unzip
  			
  			if(level(0) + start == number) list(0)
  			else Iterate(level(0) , index + 1)
  		}
  		Iterate(0,1)
  	}
  };System.out.println("""AlternativeNthPrimeWithMap: (number: Int)Int""");$skip(80); val res$0 = 
  
  (1 until 7) flatMap(i=>(1 until i).filter(j=> IsPrime(i+j)).map(j=>(i,j)));System.out.println("""res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$0));$skip(78); val res$1 = 
  
  for (i <- 1 until 7; j <- 1 until i if (IsPrimeGeneric(i+j))) yield(i,j);System.out.println("""res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = """ + $show(res$1));$skip(12); 

val v = 10;System.out.println("""v  : Int = """ + $show(v ));$skip(83); 
val l = List(v,v+1).map(x => (x>5) match {	case true=> (x,2)	case false=> (x,8)	});System.out.println("""l  : List[(Int, Int)] = """ + $show(l ));$skip(14); val res$2 = 
  (l(1),l(0));System.out.println("""res2: ((Int, Int), (Int, Int)) = """ + $show(res$2));$skip(550); 
  
  
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
  };System.out.println("""PrimeFactorization: (number: Int)List[(Int, Int)]""");$skip(27); val res$3 = 
  
  PrimeFactorization(5);System.out.println("""res3: List[(Int, Int)] = """ + $show(res$3));$skip(25); val res$4 = 
  PrimeFactorization(24);System.out.println("""res4: List[(Int, Int)] = """ + $show(res$4));$skip(28); val res$5 = 
  PrimeFactorization(12545);System.out.println("""res5: List[(Int, Int)] = """ + $show(res$5));$skip(28); val res$6 = 
  PrimeFactorization(12549);System.out.println("""res6: List[(Int, Int)] = """ + $show(res$6));$skip(39); 
  
  val p1 = System.currentTimeMillis;System.out.println("""p1  : Long = """ + $show(p1 ));$skip(30); val res$7 = 
  PrimeFactorization(1002541);System.out.println("""res7: List[(Int, Int)] = """ + $show(res$7));$skip(36); 
  val p2 = System.currentTimeMillis;System.out.println("""p2  : Long = """ + $show(p2 ));$skip(51); 
  println("PrimeFactorization took: " + (p2 - p1));$skip(40); 
  
  val _p1 = System.currentTimeMillis;System.out.println("""_p1  : Long = """ + $show(_p1 ));$skip(30); val res$8 = 
  PrimeFactorization(1002543);System.out.println("""res8: List[(Int, Int)] = """ + $show(res$8));$skip(30); val res$9 = 
  PrimeFactorization(1002545);System.out.println("""res9: List[(Int, Int)] = """ + $show(res$9));$skip(30); val res$10 = 
  PrimeFactorization(1002547);System.out.println("""res10: List[(Int, Int)] = """ + $show(res$10));$skip(30); val res$11 = 
  PrimeFactorization(1002549);System.out.println("""res11: List[(Int, Int)] = """ + $show(res$11));$skip(37); 
  val _p2 = System.currentTimeMillis;System.out.println("""_p2  : Long = """ + $show(_p2 ));$skip(54); 
  println("PrimeFactorizations took: " + (_p2 - _p1));$skip(23); val res$12 = 

   GenericNthPrime(1);System.out.println("""res12: Int = """ + $show(res$12));$skip(22); val res$13 = 
   GenericNthPrime(5);System.out.println("""res13: Int = """ + $show(res$13));$skip(25); val res$14 = 
   GenericNthPrime(1000);System.out.println("""res14: Int = """ + $show(res$14));$skip(25); val res$15 = 
   GenericNthPrime(5000);System.out.println("""res15: Int = """ + $show(res$15));$skip(36); 
   val a = System.currentTimeMillis;System.out.println("""a  : Long = """ + $show(a ));$skip(64); 
   //GenericNthPrime(50000)
   val b = System.currentTimeMillis;System.out.println("""b  : Long = """ + $show(b ));$skip(15); val res$16 = 
   NthPrime(5);System.out.println("""res16: Int = """ + $show(res$16));$skip(18); val res$17 = 
   NthPrime(1000);System.out.println("""res17: Int = """ + $show(res$17));$skip(18); val res$18 = 
   NthPrime(5000);System.out.println("""res18: Int = """ + $show(res$18));$skip(36); 
   val c = System.currentTimeMillis;System.out.println("""c  : Long = """ + $show(c ));$skip(19); val res$19 = 
   NthPrime(50000);System.out.println("""res19: Int = """ + $show(res$19));$skip(36); 
   val d = System.currentTimeMillis;System.out.println("""d  : Long = """ + $show(d ));$skip(50); 
   println("NthPrime iteration took: " + (d - c));$skip(22); val res$20 = 
   
   NewNthPrime(5);System.out.println("""res20: Int = """ + $show(res$20));$skip(21); val res$21 = 
   NewNthPrime(1000);System.out.println("""res21: Int = """ + $show(res$21));$skip(21); val res$22 = 
   NewNthPrime(5000);System.out.println("""res22: Int = """ + $show(res$22));$skip(37); 
   val _c = System.currentTimeMillis;System.out.println("""_c  : Long = """ + $show(_c ));$skip(22); val res$23 = 
   NewNthPrime(50000);System.out.println("""res23: Int = """ + $show(res$23));$skip(37); 
   val _d = System.currentTimeMillis;System.out.println("""_d  : Long = """ + $show(_d ));$skip(55); 
   println("NewNthPrime iteration took: " + (_d - _c));$skip(28); val res$24 = 
       
   FinalNthPrime(5);System.out.println("""res24: Int = """ + $show(res$24));$skip(23); val res$25 = 
   FinalNthPrime(1000);System.out.println("""res25: Int = """ + $show(res$25));$skip(23); val res$26 = 
   FinalNthPrime(5000);System.out.println("""res26: Int = """ + $show(res$26));$skip(38); 
   val __c = System.currentTimeMillis;System.out.println("""__c  : Long = """ + $show(__c ));$skip(24); val res$27 = 
   FinalNthPrime(50000);System.out.println("""res27: Int = """ + $show(res$27));$skip(38); 
   val __d = System.currentTimeMillis;System.out.println("""__d  : Long = """ + $show(__d ));$skip(59); 
   println("FinalNthPrime iteration took: " + (__d - __c));$skip(821); 
   
   
   /*AlternativeNthPrimeWithMap(1)
   AlternativeNthPrimeWithMap(5)
   AlternativeNthPrimeWithMap(1000)
   AlternativeNthPrimeWithMap(5000)
   val e = System.currentTimeMillis
   //AlternativeNthPrimeWithMap(50000)
   val f = System.currentTimeMillis
   
   
   
   OptimedGenericNthPrime(1)
   OptimedGenericNthPrime(5)
   OptimedGenericNthPrime(1000)
   OptimedGenericNthPrime(5000)
   val g = System.currentTimeMillis
   //OptimedGenericNthPrime(50000)
   val h = System.currentTimeMillis
   
   NthPrimeWithMap(1)
   NthPrimeWithMap(5)
   NthPrimeWithMap(1000)
   NthPrimeWithMap(5000)
   val i = System.currentTimeMillis
   //NthPrimeWithMap(50000)
   val j = System.currentTimeMillis
   
   
   */
   //println("GenericNthPrime iteration took: " + (b - a))
   println("NthPrime iteration took: " + (d - c));$skip(55); 
   println("NewNthPrime iteration took: " + (_d - _c))}
   //println("AlternativeNthPrimeWithMap iteration took: " + (f - e))
   //println("OptimedGenericNthPrime iteration took: " + (h - g))
   //println("NthPrimeWithMap iteration took: " + (j - i))
   
}
