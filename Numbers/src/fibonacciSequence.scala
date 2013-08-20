object fibonacciSequence {
  
  def Fibonacci(number:Int):Int = {
  	if (number < 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
    def Loop(number:Int, acc:Int = 1, inc:Int = 0):Int = number match {
    	case 0 => acc case _ => Loop(number - 1, acc + inc, acc)
    }
    Loop(number)
  }

}