package me.chuwy.otusbats


trait Show[A] {
  def show(a: A): String = a.toString
}

object Show {

  // 1.1 Instances (`Int`, `String`, `Boolean`)

  implicit val intShow: Show[Int] = new Show[Int] {}

  implicit val stringShow: Show[String] = new Show[String] {}

  implicit val booleanShow: Show[Boolean] = new Show[Boolean] {}

  // 1.2 Instances with conditional implicit

  implicit def listShow[A](implicit ev: Show[A]): Show[List[A]] =
    new Show[List[A]] {
      override def show(a: List[A]): String = mkString_(a, ", ", "", "")
    }

  implicit def setShow[A](implicit ev: Show[A]): Show[Set[A]] =
    new Show[Set[A]] {
      override def show(a: Set[A]): String = mkString_(a, ", ", "", "")
    }

  // 3. Syntax extensions

  implicit class ShowOps[A](a: A) {
    def show(implicit ev: Show[A]): String = ev.show(a)

    // 2. Summoner (apply)

    def apply(implicit ev: Show[A]): Show[A] = ev
  }

  /** Transform list of `A` into `String` with custom separator, beginning and ending.
   *  For example: "[a, b, c]" from `List("a", "b", "c")`
   *
   *  @param separator. ',' in above example
   *  @param begin. '[' in above example
   *  @param end. ']' in above example
   */

  def mkString_[B: Show]( list: Iterable[B], separator: String, begin: String, end: String): String = {
    list.zipWithIndex
      .collect {
        case (str, 0) => str.show
        case (str, _) => separator + str.show
      }
      .foldLeft(begin)((elem, accum) => elem + accum) + end
  }
}
