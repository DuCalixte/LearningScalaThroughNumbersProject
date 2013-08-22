package com.scala.numbers.solutions

object fibonacciSequence {
  
  def fibonacci(number:Int):Int = {
  	if (number < 0) throw new java.lang.IllegalArgumentException("index must be greater than zero")
    def loop(number:Int, acc:Int = 1, inc:Int = 0):Int = number match {
    	case 0 => acc case _ => loop(number - 1, acc + inc, acc)
    }
    loop(number)
  }

}