package maf.lattice
import maf.util.*
import maf.core.*
import maf.lattice.interfaces.*
import NumOps.*
import spire.math._
import spire.implicits._

object ConstantPropagationV2:

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

    type N = L[Complex[Double]]

    object L:

        implicit val numberCP: NumberLattice[N] = new BaseInstance[Complex[Double]]("Number") with NumberLattice[N] {

            def inject(x: Complex[Double]) = Constant(x)
            def inject(x: BigInt) = inject(Complex[Double](x))
            def inject(x: Double) = inject(Complex[Double](x))

            case object ExclReal extends L[Nothing]
            case object ExclComplex extends L[Nothing]
            case object Integer extends L[Nothing]
            case object Real extends L[Nothing]

            val exclReal = ExclReal
            val exclComplex = ExclComplex
            val integer = Integer
            val real = Real

            private def isConstantInteger(n: Complex[Double]): Boolean =
                n.isReal && (scala.math.round(n.real) == n.real)

            def isInteger[B2: BoolLattice](n: N): B2 =
                n match
                    case Constant(x) => BoolLattice[B2].inject(isConstantInteger(x))
                    case Bottom => BoolLattice[B2].bottom
                    case Integer => BoolLattice[B2].inject(true)
                    case ExclReal => BoolLattice[B2].inject(false)
                    case ExclComplex => BoolLattice[B2].inject(false)
                    case _ => BoolLattice[B2].top

            private def isConstantReal(n: Complex[Double]): Boolean =
                n.isReal && (scala.math.round(n.real) != n.real)

            def isReal[B2: BoolLattice](n: N): B2 =
                n match
                    case Constant(x) => BoolLattice[B2].inject(isConstantReal(x))
                    case Bottom => BoolLattice[B2].bottom
                    case Real => BoolLattice[B2].inject(true)
                    case Integer => BoolLattice[B2].inject(true)
                    case ExclReal => BoolLattice[B2].inject(true)
                    case ExclComplex => BoolLattice[B2].inject(false)
                    case _ => BoolLattice[B2].top

            def isConstantComplex(n: Complex[Double]): Boolean =
                n.imag != 0

            def isComplex[B2: BoolLattice](n: N): B2 =
                n match
                    case Bottom => BoolLattice[B2].bottom
                    case _ => BoolLattice[B2].inject(true)

            def constantToAbstract(n: N): N =
                n match
                    case Constant(a) => 
                        if isConstantInteger(a) then 
                            Integer 
                        else if isConstantReal(a) then 
                            ExclReal 
                        else 
                            ExclComplex
                    case _ => n

            override def subsumes(x: N, y: => N): Boolean =
                if x == y then true
                else
                    (x, y) match
                        case (Bottom, _) => false
                        case (Top, _) => true
                        case (_, Top) => false
                        case (_, Bottom) => true
                        case (Constant(_), _) => false
                        case (_, Constant(_)) => subsumes(x, constantToAbstract(y))
                        case (Real, Integer) => true
                        case (Real, ExclReal) => true
                        case _ => false

            override def join(x: N, y: => N): N =
                if x == y then
                    x
                else if constantToAbstract(x) ==  constantToAbstract(y) then
                    constantToAbstract(x)
                else
                    (constantToAbstract(x), constantToAbstract(y)) match
                        case (Top, _) => Top
                        case (_, Top) => Top
                        case (Bottom, _) => y
                        case (_, Bottom) => x
                        case (ExclComplex, _) => Top
                        case (_, ExclComplex) => Top
                        case _ => Real

            def generalRound(n: N, f: Double => Double): N =
                n match
                    case Integer => Integer
                    case Real => Integer
                    case Top => Top
                    case ExclComplex => Top
                    case ExclReal => Integer
                    case Constant(x) => if !isConstantComplex(x) then Constant(Complex[Double](f(x.real))) else Top
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
                case Constant(x) => Constant(x.asin)
                case Bottom => Bottom
                case _ => Top

            def cos(n: N): N = n match
                case Constant(x) => Constant(x.cos)
                case Integer => Real
                case _ => n

            def acos(n: N): N = n match
                case Constant(x) => Constant(MathOps.acos(x))
                case Bottom => Bottom
                case _ => Top

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
                case Bottom => Bottom
                case ExclComplex => ExclComplex
                case _ => Top

            // + en -
            private def binop1(op: (Complex[Double], Complex[Double]) => Complex[Double], n1: N, n2: N) = (n1, n2) match
                case (Constant(x), Constant(y)) => Constant(op(x, y))
                case _ => (constantToAbstract(n1), constantToAbstract(n2)) match
                    case (Bottom, _) => Bottom
                    case (_, Bottom) => Bottom
                    case (Top, _) => Top
                    case (_, Top) => Top
                    case (ExclComplex, ExclComplex) => Top
                    case (ExclReal, ExclReal) => Real
                    case (ExclComplex, _) => ExclComplex
                    case (_, ExclComplex) => ExclComplex
                    case (Real, _) => Real
                    case (_, Real) => Real
                    case (Integer, n) => n
                    case (n, Integer) => n

            //*
            private def binop3(op: (Complex[Double], Complex[Double]) => Complex[Double], n1: N, n2: N) =
                (n1, n2) match
                    case (Constant(x), Constant(y)) => Constant(op(x, y))
                    case _ => (constantToAbstract(n1), constantToAbstract(n2)) match
                        case (Bottom, _) => Bottom
                        case (_, Bottom) => Bottom
                        case (Top, _) => Top
                        case (_, Top) => Top
                        case (ExclComplex, ExclComplex) => Top
                        case (ExclComplex, _) => ExclComplex
                        case (_, ExclComplex) => ExclComplex
                        case (Integer, Integer) => Integer
                        case (ExclReal, _) => ExclReal
                        case (_, ExclReal) => ExclReal
                        case _ => Real


            def plus(n1: N, n2: N): N = binop1(_ + _, n1, n2)
            def minus(n1: N, n2: N): N = binop1(_ - _, n1, n2)
            def times(n1: N, n2: N): N = binop1(_ * _, n1, n2)

            // /
            private def binop2(op: (Complex[Double], Complex[Double]) => Complex[Double], n1: N, n2: N) = (n1, n2) match
                case (Integer, Integer) => Real
                case (ExclReal, _) => Real
                case (_, ExclReal) => Real
                case _ => binop3(op, n1, n2)

            def div(n1: N, n2: N): N = binop2(_ / _, n1, n2)
            def expt(n1: N, n2: N): N = binop3((x, y) => x.pow(y), n1, n2)

            def log(n: N): N = n match
                case Constant(x) => if x == 0 then Bottom else Constant(x.log)
                case Bottom => Bottom
                case ExclComplex => ExclComplex
                case _ => Top

            def toString[S2: StringLattice](n: N): S2 = n match
                case Top => StringLattice[S2].top
                case Constant(x) => StringLattice[S2].inject(x.toString)
                case Bottom => StringLattice[S2].bottom

            def lt[B2: BoolLattice](n1: N, n2: N): B2 =
                (n1, n2) match
                    case (Constant(x), Constant(y)) => {
                        if isConstantComplex(x) || isConstantComplex(y) then
                            BoolLattice[B2].bottom
                        else
                            BoolLattice[B2].inject(x.real < y.real)
                    }
                    case (Top, _) => BoolLattice[B2].bottom
                    case (_, Top) => BoolLattice[B2].bottom
                    case (ExclComplex, _) => BoolLattice[B2].bottom
                    case (_, ExclComplex) => BoolLattice[B2].bottom
                    case (Bottom, _) => BoolLattice[B2].bottom
                    case (_, Bottom) => BoolLattice[B2].bottom
                    case (Constant(x), _) => if isConstantComplex(x) then BoolLattice[B2].bottom else BoolLattice[B2].top
                    case (_, Constant(x)) => if isConstantComplex(x) then BoolLattice[B2].bottom else BoolLattice[B2].top
                    case _ => BoolLattice[B2].top

            def RMQ(n1: N, n2: N, f: (Double, Double) => Double): N =
                (n1, n2) match
                    case (Constant(x), Constant(y)) => if isConstantInteger(x) && isConstantInteger(y) then Constant(Complex[Double](f(x.real, y.real))) else Bottom
                    case (Constant(x), Integer) => if isConstantInteger(x) then Integer else Bottom
                    case (Integer, Constant(x)) => if isConstantInteger(x) then Integer else Bottom
                    case (Integer, Integer) => Integer
                    case _ => Bottom

            def remainder(n1: N, n2: N): N = RMQ(n1, n2, (x, y) => MathOps.remainder(x.toBigInt, y.toBigInt).toDouble)
            def modulo(n1: N, n2: N): N = RMQ(n1, n2, (x, y) => MathOps.modulo(x.toBigInt, y.toBigInt).toDouble)
            def quotient(n1: N, n2: N): N = RMQ(n1, n2, (x, y) => scala.math.floorDiv(x.toInt, y.toInt).toDouble)

            override def imagPart(n: N): N =
                n match
                    case Constant(x) => Constant(x.imag)
                    case Top => Real
                    case ExclComplex => Real
                    case Bottom => Bottom
                    case _ => Constant(Complex[Double](0))

            override def realPart(n: N): N =
                n match
                    case Constant(x) => Constant(x.real)
                    case Bottom => Bottom
                    case Integer => Integer
                    case ExclReal => ExclReal
                    case _ => Real


            def toChar[C2: CharLattice](n: N): C2 = n match
                case Top => CharLattice[C2].top
                case Constant(x) => if isConstantInteger(x) then CharLattice[C2].inject(x.real.toInt.asInstanceOf[Char]) else CharLattice[C2].bottom
                case Bottom => CharLattice[C2].bottom

            def makeString[C2: CharLattice, S2: StringLattice](length: N, char: C2): S2 = (length, char) match
                case (Bottom, _) => StringLattice[S2].bottom
                case (_, bot) if bot == CharLattice[C2].bottom => StringLattice[S2].bottom
                case (Top, _) => StringLattice[S2].top
                case (Constant(n), _) =>
                    val c = CharLattice[C2].toString[S2](char)
                    1.to(n.real.toInt).foldLeft(StringLattice[S2].inject(""))((s, _) => StringLattice[S2].append(s, c))
                case _ => StringLattice[S2].bottom

        }

