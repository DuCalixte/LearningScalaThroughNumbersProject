package com.scala.numbers.calculators.complex.main

class Complex(x:Double, y:Double){
  def this(x:Double)=this(x,0)
  
  def real = x
  def imaginary = y
  
  def + (that:Complex)= new Complex(x + that.real, y + that.imaginary)
  def - (that:Complex)= new Complex(x - that.real, y - that.imaginary)
  def * (that:Complex)= new Complex((x * that.real - y * that.imaginary), (y * that.real + x * that.imaginary))
  def / (that:Complex)= new Complex(
      (x * that.real + y * that.imaginary) / (that.real * that.real + that.imaginary * that.imaginary), 
      (y * that.real - x * that.imaginary) / (that.real * that.real + that.imaginary * that.imaginary)
      )
  override def toString = y match {case 0 => x + "" case _ => x match { case 0 => y + "i" case _ => x + " + " + y + "i"}
}}