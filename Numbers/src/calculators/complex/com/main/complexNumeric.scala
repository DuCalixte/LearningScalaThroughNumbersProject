package calculators.complex.com.main

object complexNumeric {
  
  val one = new complex(1)
  val two = new complex(1,1)
  
  val three = one + two

}

class complex(x:Double, y:Double){
  def this(x:Double)=this(x,0)
  
  def real = x
  def imaginary = y
  
  def + (that:complex)= new complex(x + that.real, y + that.imaginary)
  def - (that:complex)= new complex(x - that.real, y - that.imaginary)
  def * (that:complex)= new complex((x * that.real - y * that.imaginary), (y * that.real + x * that.imaginary))
  def / (that:complex)= new complex(
      (x * that.real + y * that.imaginary) / (that.real * that.real + that.imaginary * that.imaginary), 
      (y * that.real - x * that.imaginary) / (that.real * that.real + that.imaginary * that.imaginary)
      )
  override def toString = y match {case 0 => x + "" case _ => x match { case 0 => y + "i" case _ => x + " + " + y + "i"}
}}