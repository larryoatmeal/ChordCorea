package models.utility

object MyString{


  def optionalAppend(str: String, suffix: String, condition: Boolean) = {
    if(condition) str + suffix else str
  }

  def optionalPrepend(str: String, prefix: String, condition: Boolean) = {
    if(condition) prefix + str else str
  }


}