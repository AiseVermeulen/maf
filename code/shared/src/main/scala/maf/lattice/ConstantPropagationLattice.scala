package maf.lattice

import maf.util.*
import maf.core.*
import maf.lattice.interfaces.*
import NumOps.*
import spire.math._
import spire.implicits._

object ConstantPropagation:

    sealed trait L[+A]

    case object Top extends L[Nothing]

    case class Constant[A](x: A) extends L[A]

    case object Bottom extends L[Nothing]

    abstract class BaseInstance[A: Show](typeName: String) extends Lattice[L[A]]:
        def show(x: L[A]): String = x match
            case Top         => typeName
            case Constant(x) => x.toString
            case Bottom      => s"$typeName.⊥"
        val bottom: L[A] = Bottom
        val top: L[A] = Top
        def join(x: L[A], y: => L[A]): L[A] = x match
            case Top => Top
            case Constant(_) =>
                y match
                    case Top => Top
                    case Constant(_) =>
                        if x == y then x
                        else Top
                    case Bottom => x
            case Bottom => y
        def meet(x: L[A], y: => L[A]): L[A] = x match
            case Bottom => Bottom
            case Constant(_) =>
                y match
                    case Top => x
                    case Constant(_) =>
                        if x == y then x
                        else Bottom
                    case Bottom => Bottom
            case Top => y
        def subsumes(x: L[A], y: => L[A]): Boolean = x match
            case Top => true
            case Constant(_) =>
                y match
                    case Top         => false
                    case Constant(_) => x == y
                    case Bottom      => true
            case Bottom =>
                y match
                    case Top         => false
                    case Constant(_) => false
                    case Bottom      => true
        def eql[B2: BoolLattice](n1: L[A], n2: L[A]): B2 = (n1, n2) match
            case (Top, Top)                 => BoolLattice[B2].top
            case (Top, Constant(_))         => BoolLattice[B2].top
            case (Constant(_), Top)         => BoolLattice[B2].top
            case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x == y)
            case (Bottom, _)                => BoolLattice[B2].bottom
            case (_, Bottom)                => BoolLattice[B2].bottom

    type B = L[Boolean]
    type S = L[String]
    type I = L[BigInt]
    type R = L[Double]
    type C = L[Char]
    type Sym = L[String]
    type N = L[Complex[Double]]

    object L:
        implicit val boolCP: BoolLattice[B] = new BaseInstance[Boolean]("Bool") with BoolLattice[B] {
            def inject(b: Boolean): B = Constant(b)
            def isTrue(b: B): Boolean = b match
                case Top         => true
                case Constant(x) => x
                case Bottom      => false
            def isFalse(b: B): Boolean = b match
                case Top         => true
                case Constant(x) => !x
                case Bottom      => false
            def not(b: B): B = b match
                case Top         => Top
                case Constant(x) => Constant(!x)
                case Bottom      => Bottom
        }

        implicit val stringCP: StringLattice[S] = new BaseInstance[String]("Str") with StringLattice[S] {
            def inject(x: String): S = Constant(x)
            def length[N2: NumberLattice](s: S): N2 = s match
                case Top         => NumberLattice[N2].top
                case Constant(s) => NumberLattice[N2].inject(s.length)
                case Bottom      => NumberLattice[N2].bottom
            def append(s1: S, s2: S): S = (s1, s2) match
                case (Bottom, _) | (_, Bottom)  => Bottom
                case (Top, _) | (_, Top)        => Top
                case (Constant(x), Constant(y)) => Constant(x ++ y)
            def substring[N2: NumberLattice](
                s: S,
                from: N2,
                to: N2
              ): S = (s, from, to) match
                case (Bottom, _, _)                                => Bottom
                case (_, from, _) if NumberLattice[N2].isBottom(from) => Bottom
                case (_, _, to) if NumberLattice[N2].isBottom(to)     => Bottom
                case (Top, _, _)                                   => Top
                case (Constant(s), from, to)                       =>
                    // This is duplicated code from ConcreteLattice, it should be refactored
                    (0.to(s.size)
                        .collect({
                            case from2 if BoolLattice[B].isTrue(NumberLattice[N2].eql[B](from, NumberLattice[N2].inject(from2))) =>
                                (from2
                                    .to(s.size)
                                    .collect({
                                        case to2 if BoolLattice[B].isTrue(NumberLattice[N2].eql[B](to, NumberLattice[N2].inject(to2))) =>
                                            inject(s.substring(from2, to2).nn)
                                    }))
                        })
                        .flatten)
                        .foldLeft(bottom)((s1, s2) => join(s1, s2))
            def ref[N2: NumberLattice, C2: CharLattice](s: S, i: N2): C2 = s match
                case Bottom      => CharLattice[C2].bottom
                case Top         => CharLattice[C2].top
                case Constant(x) =>
                    // This is duplicated code from ConcreteLattice, it should be refactored
                    0.to(x.length)
                        .collect({
                            case i2
                                if BoolLattice[Concrete.B]
                                    .isTrue(NumberLattice[N2].eql[Concrete.B](i, NumberLattice[N2].inject(i2))) &&
                                    i2 < x.size =>
                                CharLattice[C2].inject(x.charAt(i2))
                        })
                        .foldLeft(CharLattice[C2].bottom)((c1, c2) => CharLattice[C2].join(c1, c2))
            def set[N2: NumberLattice, C2: CharLattice](
                s: S,
                i: N2,
                c: C2
              ): S = s match
                case Bottom                                                         => Bottom
                case _ if NumberLattice[N2].isBottom(i) || CharLattice[C2].isBottom(c) => Bottom
                case Top                                                            => Top
                case Constant(str) =>
                    (i, c) match
                        case (Constant(idx: BigInt), Constant(chr: Char)) => Constant(str.updated(idx.toInt, chr))
                        // If neither the index or character are constant, don't even bother
                        case _ => Top
            def lt[B2: BoolLattice](s1: S, s2: S): B2 = (s1, s2) match
                case (Bottom, _) | (_, Bottom)  => BoolLattice[B2].bottom
                case (Top, _) | (_, Top)        => BoolLattice[B2].top
                case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x < y)
            def toSymbol[Sym2: SymbolLattice](s: S): Sym2 = s match
                case Bottom      => SymbolLattice[Sym2].bottom
                case Top         => SymbolLattice[Sym2].top
                case Constant(x) => SymbolLattice[Sym2].inject(x)

            def toNumber[N2: NumberLattice](s: S) = s match
                case Bottom        => MayFail.success(NumberLattice[N2].bottom)
                case Constant(str) => MayFail.fromOption(NumOps.bigIntFromString(str).map(NumberLattice[N2].inject))(NotANumberString)
                case Top           => MayFail.success(NumberLattice[N2].top).addError(NotANumberString)
        }

        implicit val intCP: IntLattice[I] = new BaseInstance[BigInt]("Int") with IntLattice[I] {
            def inject(x: BigInt): I = Constant(x)

            def toReal[R2: RealLattice](n: I): R2 = n match
                case Top         => RealLattice[R2].top
                case Constant(x) => RealLattice[R2].inject(x.toDouble)
                case Bottom      => RealLattice[R2].bottom

            def toComplex[N2: NumberLattice](n: I): N2 = n match
                case Top => NumberLattice[N2].top
                case Constant(x) => NumberLattice[N2].inject(Complex[Double](x.toDouble))
                case Bottom => NumberLattice[N2].bottom    

            def random(n: I): I = n match
                case Bottom => Bottom
                case _      => Top

            private def binop(
                op: (BigInt, BigInt) => BigInt,
                n1: I,
                n2: I
              ) = (n1, n2) match
                case (Top, Top)                 => Top
                case (Top, Constant(_))         => Top
                case (Constant(_), Top)         => Top
                case (Constant(x), Constant(y)) => Constant(op(x, y))
                case _                          => Bottom
            def plus(n1: I, n2: I): I = binop(_ + _, n1, n2)
            def minus(n1: I, n2: I): I = binop(_ - _, n1, n2)
            def times(n1: I, n2: I): I = binop(_ * _, n1, n2)
            def div[F: RealLattice](n1: I, n2: I): F = (n1, n2) match // TODO: when the arguments are top+bottom, the result should be bottom...
                case (Top, _) | (_, Top)                  => RealLattice[F].top
                case (Constant(x), Constant(y)) if y != 0 => RealLattice[F].inject(bigIntToDouble(x) / bigIntToDouble(y))
                // TODO: use MayFail here for when divide-by-zero occurs ...
                case _ => RealLattice[F].bottom
            def expt(n1: I, n2: I): I = binop((x, y) => Math.pow(x.toDouble, y.toDouble).toInt, n1, n2)
            def quotient(n1: I, n2: I): I = binop(_ / _, n1, n2)
            def modulo(n1: I, n2: I): I = (n1, n2) match
                case (Top, Top)                           => Top
                case (Top, Constant(_))                   => Top
                case (Constant(_), Top)                   => Top
                case (Constant(x), Constant(y)) if y != 0 => Constant(MathOps.modulo(x, y))
                case _                                    => Bottom
            def remainder(n1: I, n2: I): I = (n1, n2) match
                case (Top, Top)                           => Top
                case (Top, Constant(_))                   => Top
                case (Constant(_), Top)                   => Top
                case (Constant(x), Constant(y)) if y != 0 => Constant(MathOps.remainder(x, y))
                case _                                    => Bottom
            def lt[B2: BoolLattice](n1: I, n2: I): B2 = (n1, n2) match
                case (Top, Top)                 => BoolLattice[B2].top
                case (Top, Constant(_))         => BoolLattice[B2].top
                case (Constant(_), Top)         => BoolLattice[B2].top
                case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x < y)
                case _                          => BoolLattice[B2].bottom
            def valuesBetween(n1: I, n2: I): Set[I] = (n1, n2) match
                case (Top, _)                   => Set(Top)
                case (_, Top)                   => Set(Top)
                case (Constant(x), Constant(y)) => x.to(y).map(i => Constant(i)).toSet
                case _                          => Set()
            def makeString[C2: CharLattice, S2: StringLattice](length: I, char: C2): S2 = (length, char) match
                case (Bottom, _)                               => StringLattice[S2].bottom
                case (_, bot) if bot == CharLattice[C2].bottom => StringLattice[S2].bottom
                case (Top, _)                                  => StringLattice[S2].top
                case (Constant(n), _) =>
                    val c = CharLattice[C2].toString[S2](char)
                    1.to(NumOps.bigIntToInt(n)).foldLeft(StringLattice[S2].inject(""))((s, _) => StringLattice[S2].append(s, c))

            def toString[S2: StringLattice](n: I): S2 = n match
                case Top         => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x.toString)
                case Bottom      => StringLattice[S2].bottom
            def toChar[C2: CharLattice](n: I): C2 = n match
                case Top         => CharLattice[C2].top
                case Constant(x) => CharLattice[C2].inject(NumOps.bigIntToInt(x).asInstanceOf[Char])
                case Bottom      => CharLattice[C2].bottom
        }

        implicit val realCP: RealLattice[R] = new BaseInstance[Double]("Real") with RealLattice[R] {
            def inject(x: Double) = Constant(x)
            def toInt[I2: IntLattice](n: R): I2 = n match
                case Top         => IntLattice[I2].top
                case Constant(x) => IntLattice[I2].inject(x.toInt)
                case Bottom      => IntLattice[I2].bottom

            def toComplex[Comp2: NumberLattice](n: R): Comp2 = n match
                case Top         => NumberLattice[Comp2].top
                case Constant(x) => NumberLattice[Comp2].inject(Complex[Double](x))
                case Bottom      => NumberLattice[Comp2].bottom
                
            def ceiling(n: R): R = n match
                case Constant(x) => Constant(x.ceil)
                case _           => n
            def floor(n: R): R = n match
                case Constant(x) => Constant(x.floor)
                case _           => n
            def round(n: R): R = n match
                case Constant(x) => Constant(MathOps.round(x))
                case _           => n
            def random(n: R): R = n match
                case Constant(_) => Top
                case _           => n
            def log(n: R): R = n match // Todo: use MayFail here or support imaginary numbers.
                case Constant(x) if 0 <= x => Constant(scala.math.log(x))
                case Top                   => Top
                case _                     => Bottom
            def sin(n: R): R = n match
                case Constant(x) => Constant(scala.math.sin(x))
                case _           => n
            def asin(n: R): R = n match // TODO: use MayFail here for when x out of bounds
                case Constant(x) if -1 <= x && x <= 1 => Constant(scala.math.asin(x))
                case Top                              => Top
                case _                                => Bottom
            def cos(n: R): R = n match
                case Constant(x) => Constant(scala.math.cos(x))
                case _           => n
            def acos(n: R): R = n match // TODO: use MayFail here for when x out of bounds
                case Constant(x) if -1 <= x && x <= 1 => Constant(scala.math.acos(x))
                case Top                              => Top
                case _                                => Bottom
            def tan(n: R): R = n match // TODO: use MayFail here for when x out of bounds
                case Constant(x) =>
                    scala.math.tan(x) match
                        case Double.NaN => Bottom
                        case n          => Constant(n)
                case _ => n
            def atan(n: R): R = n match
                case Constant(x) => Constant(scala.math.atan(x))
                case _           => n
            def sqrt(n: R): R = n match
                case Constant(x) if x >= 0 => Constant(scala.math.sqrt(x))
                case Top                   => Top
                case _                     => Bottom
            private def binop(
                op: (Double, Double) => Double,
                n1: R,
                n2: R
              ) = (n1, n2) match
                case (Top, Top)                 => Top
                case (Top, Constant(_))         => Top
                case (Constant(_), Top)         => Top
                case (Constant(x), Constant(y)) => Constant(op(x, y))
                case _                          => Bottom
            def plus(n1: R, n2: R): R = binop(_ + _, n1, n2)
            def minus(n1: R, n2: R): R = binop(_ - _, n1, n2)
            def times(n1: R, n2: R): R = binop(_ * _, n1, n2)
            def div(n1: R, n2: R): R = binop(_ / _, n1, n2)
            def expt(n1: R, n2: R): R = binop((x, y) => Math.pow(x, y), n1, n2)
            def lt[B2: BoolLattice](n1: R, n2: R): B2 = (n1, n2) match
                case (Top, Top)                 => BoolLattice[B2].top
                case (Top, Constant(_))         => BoolLattice[B2].top
                case (Constant(_), Top)         => BoolLattice[B2].top
                case (Constant(x), Constant(y)) => BoolLattice[B2].inject(x < y)
                case _                          => BoolLattice[B2].bottom
            def toString[S2: StringLattice](n: R): S2 = n match
                case Top         => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x.toString)
                case Bottom      => StringLattice[S2].bottom
        }

        implicit val complexCP: NumberLattice[N] = new BaseInstance[Complex[Double]]("Complex") with NumberLattice[N] {

            def inject(x: Complex[Double]) = Constant(x)
            def inject(x: BigInt) = inject(Complex[Double](x))
            def inject(x: Double) = inject(Complex[Double](x))
            
            case object Real extends L[Nothing]
            case object Integer extends L[Nothing]
            
            val real = Real
            val integer = Integer

            def isInteger(n: Complex[Double]): Boolean =
                n.isReal && (scala.math.round(n.real) == n.real)

            def isInteger[B2: BoolLattice](n: N): B2 =
                n match
                    case Constant(x) => BoolLattice[B2].inject(isInteger(x))
                    case Bottom => BoolLattice[B2].bottom
                    case Integer => BoolLattice[B2].inject(true)
                    case _ => BoolLattice[B2].top ///???????

            def isReal(n: Complex[Double]): Boolean =
                n.isReal && (scala.math.round(n.real) != n.real)

            override def isReal[B2: BoolLattice](n: N): B2 =
                n match
                    case Constant(x) => BoolLattice[B2].inject(isReal(x))
                    case Bottom => BoolLattice[B2].bottom
                    case Real => BoolLattice[B2].inject(true)
                    case Integer => BoolLattice[B2].inject(true)
                    case _ => BoolLattice[B2].top ///???????

            def isComplex(n: Complex[Double]): Boolean =
                n.imag != 0

            override def isComplex[B2: BoolLattice](n: N): B2 =
                n match
                    case Bottom => BoolLattice[B2].bottom
                    case _ => BoolLattice[B2].inject(true) // ????

            override def subsumes(x: N, y: => N): Boolean = x match
                case Top => true
                case Real => y match
                    case Constant(z) => !isComplex(z)
                    case Top => false
                    case _ => true
                case Integer => y match
                    case Constant(z) => isInteger(z)
                    case Integer => true
                    case Bottom => true
                    case _ => false
                case Constant(_) =>
                    y match
                        case Constant(_) => x == y
                        case Bottom => true
                        case _ => false
                case Bottom =>
                    y match
                        case Bottom => true
                        case _ => false

            override def join(x: N, y: => N): N =
                (x, y) match
                    case (Top, _) => Top
                    case (_, Top) => Top
                    case (Bottom, _) => y
                    case (_, Bottom) => x
                    case (Constant(a), Constant(b)) =>
                        if a == b then x
                        else if isComplex(a) || isComplex(b) then Top
                        else if isReal(a) || isReal(b) then Real
                        else Integer
                    case (Constant(a), _) =>
                        if isComplex(a) then Top
                        else
                            y match
                                case Real => Real
                                case Integer => if isInteger(a) then Integer else Real
                    case (_, Constant(a)) =>
                        if isComplex(a) then Top
                        else
                            x match
                                case Real => Real
                                case Integer => if isInteger(a) then Integer else Real
                    case (Real, _) => Real
                    case (_, Real) => Real
                    case (Integer, Integer) => Integer

            def generalRound(n: N, f: Double => Double): N =
                n match
                    case Integer => Integer
                    case Real => Integer
                    case Top => Top // klopt dit wel, aangezien er Complexe getallen bij zijn die niet afgerond kunnen worden
                    case Constant(x) => if !isComplex(x) then Constant(Complex[Double](f(x.real)))
                                        else Top
                    case _ => Bottom

            def round(n: N): N =
                generalRound(n, elem => scala.math.round(elem))

            def floor(n: N): N =
                generalRound(n, scala.math.floor)

            def ceiling(n: N): N =
                generalRound(n, MathOps.ceil)


            def sin(n: N): N = n match
                case Constant(x) => Constant(x.sin)
                case Integer => Real
                case _ => n

            def asin(n: N): N = n match
                case Constant(x) => if isComplex(x)
                                        then Constant(x.asin)
                                    else if  -1 <= x.real && x.real <= 1 then
                                        Constant(x.asin)
                                    else
                                        Bottom
                case Integer => Real
                case _ => n

            def cos(n: N): N = n match
                case Constant(x) => Constant(x.cos)
                case Integer => Real
                case _ => n

            def acos(n: N): N = n match
                case Constant(x) => {
                    if isComplex(x) then
                        Constant(x.acos)
                    else if -1 <= x.real && x.real <= 1 then
                        Constant(x.acos)
                    else
                        Bottom
                }
                case Integer => Real
                case _ => n

            def tan(n: N): N = n match
                case Constant(x) => Constant(x.tan)
                case Integer => Real
                case _ => n

            def atan(n: N): N = n match
                case Constant(x) => Constant(x.atan)
                case Integer => Real
                case _ => n

            def sqrt(n: N): N = n match 
                case Constant(x) => Constant(x.sqrt)
                case Real => Top
                case Integer => Top
                case _ => n

            private def binop1(
                                 op: (Complex[Double], Complex[Double]) => Complex[Double],
                                 n1: N,
                                 n2: N
                             ) = (n1, n2) match
                case (Bottom, _) => Bottom
                case (_, Bottom) => Bottom
                case (Top, _) => Top
                case (_, Top) => Top
                case (Real, Constant(x)) => if isComplex(x) then Top else Real
                case (Constant(x), Real) => if isComplex(x) then Top else Real
                case (Integer, Constant(x)) => if isInteger(x) then Integer else if isReal(x) then Real else Top
                case (Constant(x), Integer) => if isInteger(x) then Integer else if isReal(x) then Real else Top
                case (Constant(x), Constant(y)) => Constant(op(x, y))
                case (Real, _) => Real
                case (_, Real) => Real
                case _ => Integer

            def plus(n1: N, n2: N): N = binop1(_ + _, n1, n2)
            def minus(n1: N, n2: N): N = binop1(_ - _, n1, n2)
            def times(n1: N, n2: N): N = binop1(_ * _, n1, n2)

            private def binop2(
                                  op: (Complex[Double], Complex[Double]) => Complex[Double],
                                  n1: N,
                                  n2: N
                              ) = (n1, n2) match
                case (Bottom, _) => Bottom
                case (_, Bottom) => Bottom
                case (Top, _) => Top
                case (_, Top) => Top
                case (Constant(x), Constant(y)) => Constant(op(x, y))
                case (Constant(x), _) => if isComplex(x) then Top else Real
                case (_, Constant(x)) => if isComplex(x) then Top else Real
                case _ => Real

            def div(n1: N, n2: N): N = binop2(_ / _, n1, n2)
            def expt(n1: N, n2: N): N = binop1((x, y) => x.pow(y), n1, n2)

            def log(n: N): N = n match
                case Constant(x) => Constant(x.log)
                case Top => Top
                case _ => n

            def toString[S2: StringLattice](n: N): S2 = n match
                case Top => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x.toString)
                case Bottom => StringLattice[S2].bottom

            def lt[B2: BoolLattice](n1: N, n2: N): B2 =
                (n1, n2) match
                    case (Constant(x), Constant(y)) => {
                        if isComplex(x) || isComplex(y) then
                            BoolLattice[B2].bottom
                        else
                            BoolLattice[B2].inject(x.real < y.real)
                    }
                    case (Top, _) => BoolLattice[B2].bottom
                    case (_, Top) => BoolLattice[B2].bottom
                    case (Bottom, _) => BoolLattice[B2].bottom
                    case (_, Bottom) => BoolLattice[B2].bottom
                    case (Constant(x), _) => if isComplex(x) then BoolLattice[B2].bottom else BoolLattice[B2].top
                    case (_, Constant(x)) => if isComplex(x) then BoolLattice[B2].bottom else BoolLattice[B2].top
                    case _ => BoolLattice[B2].top

            def RMQ(n1: N, n2: N, f: (Double, Double) => Double): N =
                (n1, n2) match
                    case (Constant(x), Constant(y)) => if isInteger(x) && isInteger(y) then Constant(Complex[Double](f(x.real, y.real))) else Bottom
                    case (Constant(x), Integer) => if isInteger(x) then Integer else Bottom
                    case (Integer, Constant(x)) => if isInteger(x) then Integer else Bottom
                    case (Integer, Integer) => Integer
                    case _ => Bottom

            def remainder(n1: N, n2: N): N = RMQ(n1, n2, (x, y) => MathOps.remainder(x.toBigInt, y.toBigInt).toDouble)
            def modulo(n1: N, n2: N): N = RMQ(n1, n2, (x, y) => MathOps.modulo(x.toBigInt, y.toBigInt).toDouble)
            def quotient(n1: N, n2: N): N = RMQ(n1, n2, (x, y) => scala.math.floorDiv(x.toInt, y.toInt).toDouble)

        }

        implicit val charCP: CharLattice[C] = new BaseInstance[Char]("Char") with CharLattice[C] {
            def inject(x: Char) = Constant(x)
            def downCase(c: C): C = c match
                case Constant(char) => Constant(char.toLower)
                case _              => c
            def upCase(c: C): C = c match
                case Constant(char) => Constant(char.toUpper)
                case _              => c
            def toString[S2: StringLattice](c: C): S2 = c match
                case Top            => StringLattice[S2].top
                case Constant(char) => StringLattice[S2].inject(char.toString)
                case Bottom         => StringLattice[S2].bottom
            def toInt[N2: NumberLattice](c: C): N2 = c match
                case Bottom      => NumberLattice[N2].bottom
                case Constant(c) => NumberLattice[N2].inject(c.toInt)
                case Top         => NumberLattice[N2].top
            def isLower[B2: BoolLattice](c: C): B2 = c match
                case Bottom         => BoolLattice[B2].bottom
                case Constant(char) => BoolLattice[B2].inject(char.isLower)
                case Top            => BoolLattice[B2].top
            def isUpper[B2: BoolLattice](c: C): B2 = c match
                case Bottom         => BoolLattice[B2].bottom
                case Constant(char) => BoolLattice[B2].inject(char.isUpper)
                case Top            => BoolLattice[B2].top
            override def charEq[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom)    => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) => BoolLattice[B2].inject(c1 == c2)
                case _                            => BoolLattice[B2].top
            override def charLt[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom)    => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) => BoolLattice[B2].inject(c1 < c2)
                case _                            => BoolLattice[B2].top
            override def charEqCI[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom) => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) =>
                    BoolLattice[B2].inject(c1.toUpper == c2.toUpper) // TODO implement better (see note in concrete lattice)
                case _ => BoolLattice[B2].top
            override def charLtCI[B2: BoolLattice](c1: C, c2: C): B2 = (c1, c2) match
                case (Bottom, _) | (_, Bottom) => BoolLattice[B2].bottom
                case (Constant(c1), Constant(c2)) =>
                    BoolLattice[B2].inject(c1.toUpper < c2.toUpper) // TODO implement better (see note in concrete lattice)
                case _ => BoolLattice[B2].top
        }

        implicit val symCP: SymbolLattice[Sym] = new BaseInstance[String]("Symbol")(Show.symShow) with SymbolLattice[Sym] {
            def inject(x: String) = Constant(x)
            def toString[S2: StringLattice](s: Sym): S2 = s match
                case Top         => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x)
                case Bottom      => StringLattice[S2].bottom
        }
