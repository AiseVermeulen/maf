package maf.util

import maf.core.*
import spire.math.Complex

trait Show[V] extends Serializable:
    def show(v: V): String

object Show:
    def apply[V: Show]: Show[V] = implicitly

    implicit val stringShow: Show[String] = new Show[String] {
        def show(s: String): String = s""""$s""""
    }
    implicit val boolShow: Show[Boolean] = new Show[Boolean] {
        def show(b: Boolean): String =
            if b then "#t"
            else "#f"
    }
    implicit val intShow: Show[BigInt] = new Show[BigInt] {
        def show(i: BigInt): String = s"$i"
    }
    implicit val doubleShow: Show[Double] = new Show[Double] {
        def show(d: Double): String = s"$d"
    }
    implicit val charShow: Show[Char] = new Show[Char] {
        def show(c: Char): String = s"#\\$c"
    }
    implicit val compShow: Show[Complex[Double]] = new Show[Complex[Double]] {
        def show(c: Complex[Double]): String = s"$c"
    }
    given listShow[X: Show]: Show[List[X]] with
        def show(v: List[X]): String =
            s"[${v.map(Show[X].show).mkString(",")}]"

    // Not implicit because it would conflict with stringShow.
    val symShow: Show[String] = new Show[String] {
        def show(s: String): String = s"'$s"
    }
