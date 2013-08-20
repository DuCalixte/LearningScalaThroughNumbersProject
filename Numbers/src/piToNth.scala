object piToNth {
  
  def PiToNth(size:Int){
    val pi = 4.0 * (4.0 * math.atan(1.0/5.0) - math.atan(1.0/239.0))
    printf("%1." + size + "f\n", pi)
  }

}