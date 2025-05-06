package maf.modular.scheme

import maf.core._
import maf.modular._
import maf.lattice._
import maf.language.scheme._
import maf.language.scheme.lattices.{ModularSchemeLattice, SchemeLattice}
import maf.language.scheme.primitives._

/** The abstract domain used for Scheme analyses. */
trait SchemeDomain extends AbstractDomain[SchemeExp]:

    /** Type of primitive functions. */
    type Prim = SchemePrimitive[Value, Address]

    /** Contains the implementation of the primitives for the given abstract domain. */
    lazy val primitives: SchemePrimitives[Value, Address]

    /** Implementation of abstract values. */
    implicit lazy val lattice: SchemeLattice[Value, Address]

trait ProvideModularLattice extends SchemeDomain:
    type S
    type B
    type C
    type Sym
    type N

    lazy val modularLattice: ModularSchemeLattice[Address, S, B, C, Sym, N]

    def convertValue(baseDomain: ModularSchemeLattice[Address, S, B, C, Sym, N])(value: Value): baseDomain.L

trait ModularSchemeDomain extends SchemeDomain:
    val modularLatticeWrapper: ModularSchemeLatticeWrapper
    // extracts value and lattice definition from this wrapper
    type Value = modularLatticeWrapper.modularLattice.L
    type ValueElement = modularLatticeWrapper.modularLattice.Value

    type S = modularLatticeWrapper.S
    type B = modularLatticeWrapper.B
    type C = modularLatticeWrapper.C
    type Sym = modularLatticeWrapper.Sym

    final lazy val modularLattice = modularLatticeWrapper.modularLattice
    final lazy val lattice = modularLatticeWrapper.modularLattice.schemeLattice
    final lazy val primitives = modularLatticeWrapper.primitives

trait ModularSchemeLatticeWrapper:
    // consists of several types
    type S
    type B
    type C
    type Sym
    type N
    // holds a modular lattice
    val modularLattice: ModularSchemeLattice[Address, S, B, C, Sym, N]
    val primitives: SchemePrimitives[modularLattice.L, Address]

//
// TYPE DOMAIN
//

/* A type lattice for ModF */
object SchemeTypeDomain extends ModularSchemeLatticeWrapper:
    // use type domains everywhere, except for booleans
    type S = Type.S
    type B = ConstantPropagation.B
    type C = Type.C
    type Sym = Type.Sym
    type N = Type.N
    // make the scheme lattice
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait SchemeTypeDomain extends ModularSchemeDomain:
    val modularLatticeWrapper = SchemeTypeDomain
    override def domainName: String = "modular type Scheme domain"

//
// CONSTANT PROPAGATION DOMAIN
//

object SchemeConstantPropagationDomain extends ModularSchemeLatticeWrapper:
    // use constant propagation domains everywhere, except for booleans
    type S = ConstantPropagation.S
    type B = ConstantPropagation.B
    type C = ConstantPropagation.C
    type Sym = ConstantPropagation.Sym
    type N = ConstantPropagation.N
    // make the scheme lattice
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait SchemeConstantPropagationDomain extends ModularSchemeDomain:
    val modularLatticeWrapper = SchemeConstantPropagationDomain
    override def domainName: String = "modular constant propagation Scheme domain"

object SchemeConstantPropagationDomainV2 extends ModularSchemeLatticeWrapper:
    // use constant propagation domains everywhere, except for booleans
    type S = ConstantPropagation.S
    type B = ConstantPropagation.B
    type C = ConstantPropagation.C
    type Sym = ConstantPropagation.Sym
    type N = ConstantPropagationV2.N
    // make the scheme lattice
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait SchemeConstantPropagationDomainV2 extends ModularSchemeDomain:
    val modularLatticeWrapper = SchemeConstantPropagationDomainV2

    override def domainName: String = "modular constant propagation Scheme domain"    

//
// POWERSET DOMAIN
//

/* A powerset lattice for ModF */
object SchemePowersetDomain extends ModularSchemeLatticeWrapper:
    // use powerset domains everywhere
    type S = Concrete.S
    type B = Concrete.B
    type C = Concrete.C
    type Sym = Concrete.Sym
    type N = Concrete.N
    // make the scheme lattice
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait SchemePowersetDomain extends ModularSchemeDomain:
    val modularLatticeWrapper = SchemePowersetDomain
    override def domainName: String = "modular powerset Scheme domain"

//
// BOUNDED SET DOMAIN
//
class SchemeBoundedDomainWrapper(val bound: Int) extends ModularSchemeLatticeWrapper:
    object Bounded extends BoundedLattice(bound)
    type S = Bounded.S
    type B = Bounded.B
    type C = Bounded.C
    type Sym = Bounded.Sym
    type N = Bounded.N
    final val modularLattice = new ModularSchemeLattice
    final val primitives = new SchemeLatticePrimitives()(modularLattice.schemeLattice)

trait SchemeBoundedDomain(bound: Int) extends ModularSchemeDomain:
    val modularLatticeWrapper = new SchemeBoundedDomainWrapper(bound)
    override def domainName: String = "modular bounded Scheme domain"
