package models.utility

object MyMath{
  def mod(a: Integer, n: Integer) = {
    ((a % n) + n) % n
  }
  def quotient(a: Integer, b: Integer) = {
    a / b
  }
  // def remAndQuotient(a: Integer, n: Integer) = {
  //   Tuple2(a%n,quotient(a,n)) //a%n preserve sign
  // }
}
