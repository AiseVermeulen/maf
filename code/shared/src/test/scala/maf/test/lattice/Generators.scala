package maf.test.lattice

import org.scalacheck.*
import maf.core.Lattice
import maf.language.scheme.lattices.{Bottom, E, Exact, Inexact, Top}
import maf.lattice.*
import maf.lattice.interfaces.{BoolLattice, IntLattice, NumberLattice, RealLattice}
import org.apache.xmpbox.`type`.ComplexPropertyContainer
import spire.math.*
import spire.implicits.*

trait LatticeGenerator[L]:
    def any: Gen[L]
    def le(l: L): Gen[L]
    implicit val anyArb: Arbitrary[L] = Arbitrary(any)
    implicit val shrink: Shrink[L] = Shrink(v => Stream.empty)

object Generators:
    val str: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr, Gen.numStr))
    val int: Gen[BigInt] = Gen.choose(-1000, 1000)
    val double: Gen[Double] = Gen.choose(-1000.0, 1000.0)
    val char: Gen[Char] = Gen.choose(0.toChar, 255.toChar)
    val sym: Gen[String] = Gen.resize(10, Gen.oneOf(Gen.identifier, Gen.alphaStr))
    val complex: Gen[Complex[Double]] = Gen.resultOf[Double, Double, Complex[Double]]((x, y) => Complex[Double](x, y))
   
class BooleanGenerator[B: BoolLattice] extends LatticeGenerator[B]:

    /** ConcreteBool is a finite lattice with four elements */
    val bot = BoolLattice[B].bottom
    val t = BoolLattice[B].inject(true)
    val f = BoolLattice[B].inject(false)
    val top = BoolLattice[B].top

    def any = Gen.oneOf(bot, t, f, top)
    def le(l: B) =
        if l == bot then { Gen.const(bot) }
        else if l == top then { Gen.oneOf(bot, t, f) }
        else { Gen.oneOf(l, bot) }
object ConcreteBooleanGenerator extends BooleanGenerator[Concrete.B]

case class SetGen[A](g: Gen[A]):
    /*implicit val buildable = new org.scalacheck.maf.util.Buildable[A, Set[A]] {
    def builder = new scala.collection.mutable.Builder[A, Set[A]] {
      var buff: Set[A] = Set.empty
      def clear() = { buff = Set.empty }
      def +=(x: A) = { buff = buff.union(Set(x)); this }
      def result = buff
    }
  }
  implicit val toTraversable = (s: Set[A]) => new Traversable[A] {
    def foreach[U](f: A => U): Unit = s.foreach({ x => f(x) })
  }*/
    val gen: Gen[Set[A]] = for
        n <- Gen.choose(0, 10)
        s <- Gen.buildableOfN[Set[A], A](n, g)
    yield s
    def genSubset(set: Set[A]): Gen[Set[A]] =
        val list = set.toList
        for n <- Gen.choose(0, set.size) yield scala.util.Random.shuffle(list).take(n).toSet

class ConcreteGenerator[T](g: Gen[T])(implicit lat: Lattice[Concrete.L[T]]) extends LatticeGenerator[Concrete.L[T]]:
    val isetgen = SetGen[T](g)
    val topgen: Gen[Concrete.L[T]] = lat.top

    def any: Gen[Concrete.L[T]] = Gen.oneOf(topgen, isetgen.gen.map(x => Concrete.Values(x)))
    def le(l: Concrete.L[T]) = l match
        case Concrete.Top             => any
        case Concrete.Values(content) => isetgen.genSubset(content.toSet[T]).map(x => Concrete.Values(x))
    override val shrink = Shrink {
        case Concrete.Top => Stream.empty
        case Concrete.Values(vs) =>
            Shrink.shrinkContainer[Set, T].shrink(vs.toSet[T]).map(Concrete.Values(_))
    }

object ConcreteStringGenerator extends ConcreteGenerator[String](Generators.str)(Concrete.L.stringConcrete)

object ConcreteIntGenerator extends ConcreteGenerator[BigInt](Generators.int)
object ConcreteNumberGenerator extends ConcreteGenerator[Complex[Double]](Generators.complex)
object ConcreteRealGenerator extends ConcreteGenerator[Double](Generators.double)
object ConcreteCharGenerator extends ConcreteGenerator[Char](Generators.char)

object ConcreteSymbolGenerator extends ConcreteGenerator[String](Generators.sym)(Concrete.L.symConcrete)

class NumberGenerator[N: NumberLattice](EGen: LatticeGenerator[E], NGen: LatticeGenerator[N]) extends LatticeGenerator[(E, N)]:
    override def any: Gen[(E, N)] = Gen.zip(EGen.any, NGen.any)
    override def le(l: (E, N)): Gen[(E, N)] = Gen.zip(EGen.le(l._1), NGen.le(l._2))
    
//object ConcreteNumericTowerGenerator extends NumericTowerGenerator[Concrete.I, Concrete.R, Concrete.Comp](ConcreteIntGenerator, ConcreteRealGenerator, ConcreteComplexGenerator)
    
object ExactLatticeGenerator extends LatticeGenerator[E]: 
    override def any: Gen[E] = Gen.oneOf(Exact, Inexact, Top, Bottom)
    override def le(l: E): Gen[E] =
        if l == Top then
            Gen.oneOf(Exact, Inexact, Bottom)
        else if l == Exact then
            Gen.oneOf(Exact, Bottom)
        else if l == Inexact then
            Gen.oneOf(Inexact, Bottom)
        else 
            Gen.const(Bottom)
            

object TypeGenerator extends LatticeGenerator[Type.T]:

    /** Type lattice is a finite lattice with two elements */
    def any = Gen.oneOf(Type.Top, Type.Bottom)
    def le(l: Type.T) = l match
        case Type.Top    => any
        case Type.Bottom => Gen.const(Type.Bottom)

abstract class ConstantPropagationGenerator[X](gen: Gen[X])(implicit lat: Lattice[ConstantPropagation.L[X]])
    extends LatticeGenerator[ConstantPropagation.L[X]]:
    def constgen: Gen[ConstantPropagation.L[X]] = for x <- gen yield ConstantPropagation.Constant(x)
    def botgen: Gen[ConstantPropagation.L[X]] = lat.bottom
    def topgen: Gen[ConstantPropagation.L[X]] = lat.top
    def any: Gen[ConstantPropagation.L[X]] = Gen.oneOf(constgen, botgen, topgen)
    def le(l: ConstantPropagation.L[X]) = if l == lat.top then { any }
    else if l == lat.bottom then { botgen }
    else { Gen.oneOf(l, lat.bottom) }

object ConstantPropagationStringGenerator extends ConstantPropagationGenerator[String](Generators.str)(ConstantPropagation.L.stringCP)

object ConstantPropagationIntGenerator extends ConstantPropagationGenerator[BigInt](Generators.int)

object ConstantPropagationNumberGenerator extends LatticeGenerator[ConstantPropagation.L[Complex[Double]]]:
    val lat = NumberLattice[ConstantPropagation.N]
    def constcompgen: Gen[ConstantPropagation.L[Complex[Double]]] = for x <- Generators.complex yield ConstantPropagation.Constant(x)
    def constintgen: Gen[ConstantPropagation.L[Complex[Double]]] = for x <- Generators.int yield ConstantPropagation.Constant(Complex[Double](x.toDouble, 0))
    def constrealgen: Gen[ConstantPropagation.L[Complex[Double]]] = for x <- Generators.double yield ConstantPropagation.Constant(Complex[Double](x, 0))
    def botgen: Gen[ConstantPropagation.L[Complex[Double]]] = lat.bottom
    def topgen: Gen[ConstantPropagation.L[Complex[Double]]] = lat.top
    def realgen: Gen[ConstantPropagation.L[Complex[Double]]] = lat.real
    def intgen: Gen[ConstantPropagation.L[Complex[Double]]] = lat.integer
    def any: Gen[ConstantPropagation.L[Complex[Double]]] =
        Gen.oneOf(constrealgen, constintgen, constcompgen, realgen, intgen, topgen, botgen)
    def le(l: ConstantPropagation.L[Complex[Double]]): Gen[ConstantPropagation.L[Complex[Double]]] =
        if l == lat.top then
            any
        else if l == lat.real then
            Gen.oneOf(constrealgen, constintgen, realgen, intgen, botgen)
        else if l == lat.integer then
            Gen.oneOf(constintgen, intgen, botgen)
        else if l == lat.bottom then
            botgen
        else
            Gen.oneOf(l, lat.bottom)

object ConstantPropagationNumberGeneratorV2 extends LatticeGenerator[ConstantPropagationV2.L[Complex[Double]]]:
    val lat = NumberLattice[ConstantPropagationV2.N]
    def constcompgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = for x <- Generators.complex yield ConstantPropagationV2.Constant(x)
    def constintgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = for x <- Generators.int yield ConstantPropagationV2.Constant(Complex[Double](x.toDouble, 0))
    def constrealgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = for x <- Generators.double yield ConstantPropagationV2.Constant(Complex[Double](x, 0))
    def botgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = lat.bottom
    def topgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = lat.top
    def realgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = lat.real
    def intgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = lat.integer
    def exclrealgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = lat.exclReal
    def exclcompgen: Gen[ConstantPropagationV2.L[Complex[Double]]] = lat.exclComplex
    def any: Gen[ConstantPropagationV2.L[Complex[Double]]] =
        Gen.oneOf(constrealgen, constintgen, constcompgen, realgen, intgen, topgen, botgen, exclrealgen, exclcompgen)
    def le(l: ConstantPropagationV2.L[Complex[Double]]): Gen[ConstantPropagationV2.L[Complex[Double]]] =
        if l == lat.top then
            any
        else if l == lat.exclComplex then
            Gen.oneOf(constcompgen, exclcompgen, botgen)
        else if l == lat.exclReal then
            Gen.oneOf(constrealgen, exclrealgen, botgen)
        else if l == lat.real then
            Gen.oneOf(constrealgen, constintgen, realgen, intgen, botgen, exclrealgen)
        else if l == lat.integer then
            Gen.oneOf(constintgen, intgen, botgen)
        else if l == lat.bottom then
            botgen
        else
            Gen.oneOf(l, lat.bottom)


object ConstantPropagationRealGenerator extends ConstantPropagationGenerator[Double](Generators.double)
object ConstantPropagationCharGenerator extends ConstantPropagationGenerator[Char](Generators.char)
object ConstantPropagationSymbolGenerator extends ConstantPropagationGenerator[String](Generators.sym)(ConstantPropagation.L.symCP)

object ModularNumberGenerator extends NumberGenerator[ConstantPropagation.N](ExactLatticeGenerator, ConstantPropagationNumberGenerator)
object ModularNumberGeneratorV2 extends NumberGenerator[ConstantPropagationV2.N](ExactLatticeGenerator, ConstantPropagationNumberGeneratorV2)