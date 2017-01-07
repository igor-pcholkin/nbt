package com.random.nbt
import scalaz._, Scalaz._

object Shows {

  implicit val arrayShow = Show.shows[Array[String]] {
    "Array(" + _.mkString(",") + ")"
  }

  implicit val anyShow = Show.shows[Any] { value =>
    if (value.isInstanceOf[Array[String]]) {
      arrayShow.shows(value.asInstanceOf[Array[String]])
    } else {
      value.toString
    }
  }

}
