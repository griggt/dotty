package dotty.tools.dotc
package core

import Periods._, Contexts._, Symbols._, Denotations._, Names._, Annotations._
import Types._, Flags._, Decorators._, Transformers._
import Scopes.Scope
import collection.mutable
import collection.immutable.BitSet

object SymDenotations {

  /** A denotation represents the contents of a definition
   *  during a period.
   */
  abstract class SymDenotation(initFlags: FlagSet) extends SingleDenotation {

    def owner: Symbol

    def name: Name

    def symbol: Symbol

    def info: Type

    private[this] var _flags: FlagSet = initFlags

    def flags: FlagSet = _flags

    def flags_=(flags: FlagSet): Unit =
      _flags |= flags

    def setFlags(flags: FlagSet): Unit =
      _flags |= flags

    def resetFlags(flags: FlagSet): Unit =
      _flags &~= flags

    private[this] var _privateWithin: Symbol = NoSymbol

    def privateWithin: Symbol = _privateWithin

    def privateWithin_=(sym: Symbol): Unit =
      _privateWithin = sym

    final def isLoaded = _privateWithin != null

    private[this] var _annotations: List[Annotation] = Nil

    def annotations: List[Annotation] = _annotations

    def annotations_=(annots: List[Annotation]): Unit =
      _annotations = annots

    final def isCompleted = _annotations != null

    /** is this symbol a class? */
    def isClass: Boolean = false

    /** is this symbol a method? */
    def isMethod: Boolean = false

    /** is this symbol the result of an erroneous definition? */
    def isError: Boolean = false

    def withType(tp: Type): SymDenotation = ???

    override protected def copy(s: Symbol, i: Type): SingleDenotation = new UniqueRefDenotation(s, i, validFor)
  }

  class CompleteSymDenotation(
      val symbol: Symbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      val info: Type
    ) extends SymDenotation(initFlags)

  trait LazyCompletion extends SymDenotation {
    privateWithin = null
    annotations = null

    override final def flags = {
      if (!isLoaded) tryLoad()
      super.flags
    }

    override final def privateWithin = {
      if (!isLoaded) tryLoad()
      super.privateWithin
    }

    override final def annotations: List[Annotation] = {
      val annots = super.annotations
      if (annots != null) annots else { tryComplete(); annotations }
    }

    protected def tryLoad(): Unit = try {
      if (flags is Locked) throw new CyclicReference(symbol)
      setFlags(Locked)
      load()
    } catch {
      case ex: CyclicReference => handleCycle()
    } finally {
      flags &~= Locked
    }

    protected def tryComplete() = try {
      if (flags is Locked) throw new CyclicReference(symbol)
      complete()
    } catch {
      case ex: CyclicReference => handleCycle()
    } finally {
      flags &~= Locked
    }

    protected def handleCycle(): Unit
    protected def load(): Unit
    protected def complete(): Unit
  }

  abstract class LazySymDenotation(
      val symbol: Symbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet
    ) extends SymDenotation(initFlags) with LazyCompletion {

    private var currentInfo: Type = null

    override def info = {
      if (currentInfo == null) complete()
      currentInfo
    }
  }

  abstract class ClassDenotation(initFlags: FlagSet)(implicit ctx: Context)
      extends SymDenotation(initFlags) {
    import NameFilter._
    import util.LRU8Cache

    val symbol: ClassSymbol

    def typeParams: List[TypeSymbol]

    def parents: List[Type]

    def decls: Scope

    val info = ClassInfo(owner.thisType, this)

    private var memberCacheVar: LRU8Cache[Name, DenotationSet] = null

    private def memberCache: LRU8Cache[Name, DenotationSet] = {
      if (memberCacheVar == null) memberCacheVar = new LRU8Cache
      memberCacheVar
    }

    private var thisTypeCache: ThisType = null

    def thisType(implicit ctx: Context): Type = {
      if (thisTypeCache == null)
        thisTypeCache = ThisType(symbol)
      thisTypeCache
    }

    private var typeConstructorCache: Type = null

    def typeConstructor(implicit ctx: Context): Type = {
      if (typeConstructorCache == null)
        typeConstructorCache = NamedType(thisType, symbol.name)
      typeConstructorCache
    }

    private var typeTemplateCache: Type = null

    def typeTemplate(implicit ctx: Context): Type = {
      if (typeTemplateCache == null)
        AppliedType.make(typeConstructor, typeParams map (_.typeConstructor))
      typeTemplateCache
    }

    private var baseClassesVar: List[ClassSymbol] = null
    private var superClassBitsVar: BitSet = null

    private def computeSuperClassBits(implicit ctx: Context): Unit = {
      val seen = new mutable.BitSet
      val locked = new mutable.BitSet
      def addBaseClasses(bcs: List[ClassSymbol], to: List[ClassSymbol])
          : List[ClassSymbol] = bcs match {
        case bc :: bcs1 =>
          val id = bc.superId
          if (seen contains id) to
          else if (locked contains id) throw new CyclicReference(symbol)
          else {
            locked += id
            val bcs1added = addBaseClasses(bcs1, to)
            seen += id
            if (bcs1added eq bcs1) bcs else bc :: bcs1added
          }
        case _ =>
          to
      }
      def addParentBaseClasses(ps: List[Type], to: List[ClassSymbol]): List[ClassSymbol] = ps match {
        case p :: ps1 =>
          addBaseClasses(p.baseClasses, addParentBaseClasses(ps1, to))
        case _ =>
          to
      }
      baseClassesVar = symbol :: addParentBaseClasses(parents, Nil)
      superClassBitsVar = ctx.root.uniqueBits.findEntryOrUpdate(seen.toImmutable)
    }

    def superClassBits(implicit ctx: Context): BitSet = {
      if (superClassBitsVar == null) computeSuperClassBits
      superClassBitsVar
    }

    def baseClasses(implicit ctx: Context): List[ClassSymbol] = {
      if (baseClassesVar == null) computeSuperClassBits
      baseClassesVar
    }

    /** Is this class a subclass of `clazz`? */
    final def isSubClass(clazz: ClassSymbol)(implicit ctx: Context): Boolean = {
      superClassBits contains clazz.superId
    }

    private var definedFingerPrintCache: FingerPrint = null

    private def computeDefinedFingerPrint(implicit ctx: Context): FingerPrint = {
      var bits = newNameFilter
      var e = decls.lastEntry
      while (e != null) {
        includeName(bits, name)
        e = e.prev
      }
      var ps = parents
      while (ps.nonEmpty) {
        val parent = ps.head.typeSymbol
        parent.denot match {
          case classd: ClassDenotation =>
            includeFingerPrint(bits, classd.definedFingerPrint)
            parent.denot.setFlags(Frozen)
          case _ =>
        }
        ps = ps.tail
      }
      definedFingerPrintCache = bits
      bits
    }

    /** Enter a symbol in current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def enter(sym: Symbol)(implicit ctx: Context) = {
      require(!(this is Frozen))
      decls enter sym
      if (definedFingerPrintCache != null)
        includeName(definedFingerPrintCache, sym.name)
      if (memberCacheVar != null)
        memberCache invalidate sym.name
    }

    /** Delete symbol from current scope.
     *  Note: We require that this does not happen after the first time
     *  someone does a findMember on a subclass.
     */
    def delete(sym: Symbol)(implicit ctx: Context) = {
      require(!(this is Frozen))
      decls unlink sym
      if (definedFingerPrintCache != null)
        computeDefinedFingerPrint
      if (memberCacheVar != null)
        memberCache invalidate sym.name
    }

    def definedFingerPrint(implicit ctx: Context): FingerPrint = {
      val fp = definedFingerPrintCache
      if (fp != null) fp else computeDefinedFingerPrint
    }

    final def membersNamed(name: Name)(implicit ctx: Context): DenotationSet = {
      var refs: DenotationSet = memberCache lookup name
      if (refs == null) {
        if (containsName(definedFingerPrint, name)) {
          val ownDenots = decls.denotsNamed(name)
          refs = ownDenots
          var ps = parents
          while (ps.nonEmpty) {
            val parentSym = ps.head.typeSymbol
            parentSym.denot match {
              case parentd: ClassDenotation =>
                refs = refs union
                  parentd.membersNamed(name)
                    .filterExcluded(Flags.Private)
                    .asSeenFrom(thisType, parentSym)
                    .filterDisjoint(ownDenots)
              case _ =>
            }
          }
        } else {
          refs = NoDenotation
        }
        memberCache enter (name, refs)
      }
      refs
    }

    private var baseTypeCache: java.util.HashMap[CachedType, Type] = null
    private var baseTypeValid: RunId = NoRunId

    final def baseTypeOf(tp: Type)(implicit ctx: Context): Type = {

      def computeBaseTypeOf(tp: Type): Type = tp match {
        case AppliedType(tycon, args) =>
          baseTypeOf(tycon).subst(tycon.typeParams, args)
        case tp: TypeProxy =>
          baseTypeOf(tp.underlying)
        case AndType(tp1, tp2) =>
          baseTypeOf(tp1) & baseTypeOf(tp2)
        case OrType(tp1, tp2) =>
          baseTypeOf(tp1) | baseTypeOf(tp2)
        case tp @ ClassInfo(pre, classd) =>
          def reduce(bt: Type, ps: List[Type]): Type = ps match {
            case p :: ps1 => reduce(bt & baseTypeOf(p), ps1)
            case _ => bt
          }
          if (classd.symbol == symbol) tp.typeTemplate
          else reduce(NoType, classd.parents).substThis(classd.symbol, tp.prefix)
      }

      if (symbol.isStaticMono) symbol.typeConstructor
      else tp match {
        case tp: CachedType =>
          if (baseTypeValid != ctx.runId) {
            baseTypeCache = new java.util.HashMap[CachedType, Type]
            baseTypeValid = ctx.runId
          }
          var basetp = baseTypeCache get tp
          if (basetp == null) {
            baseTypeCache.put(tp, NoType)
            basetp = computeBaseTypeOf(tp)
            baseTypeCache.put(tp, basetp)
          } else if (basetp == NoType) {
            throw new CyclicReference(symbol)
          }
          basetp
        case _ =>
          computeBaseTypeOf(tp)
      }
    }

    private var memberNamesCache: Map[NameFilter, Set[Name]] = Map()

    def memberNames(keepOnly: NameFilter)(implicit ctx: Context): Set[Name] =
      memberNamesCache get keepOnly match {
        case Some(names) =>
          names
        case _ =>
          val inheritedNames = (parents flatMap (_.memberNames(thisType, keepOnly))).toSet
          val ownNames = decls.iterator map (_.name)
          val candidates = inheritedNames ++ ownNames
          val names = candidates filter (keepOnly(thisType, _))
          memberNamesCache += (keepOnly -> names)
          names
    }
  }

  class CompleteClassDenotation(
      val symbol: ClassSymbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet,
      val typeParams: List[TypeSymbol],
      val parents: List[Type],
      val decls: Scope
    )(implicit ctx: Context) extends ClassDenotation(initFlags)

  abstract class LazyClassDenotation(
      val symbol: ClassSymbol,
      val owner: Symbol,
      val name: Name,
      initFlags: FlagSet
    )(implicit ctx: Context) extends ClassDenotation(initFlags) with LazyCompletion {

    protected var _typeParams: List[TypeSymbol] = null
    protected var _parents: List[Type] = null
    protected var _decls: Scope = null

    final def typeParams: List[TypeSymbol] = {
      val tparams = _typeParams
      if (tparams != null) tparams else { tryLoad(); typeParams }
    }

    final def parents: List[Type] = {
      val ps = _parents
      if (ps != null) ps else { tryComplete(); parents }
    }

    final def decls: Scope = {
      val ds = _decls
      if (ds != null) ds else { tryComplete(); decls }
    }
  }

  object NoDenotation extends SymDenotation(Flags.Empty) {
    override def symbol: Symbol = NoSymbol
    override def owner: Symbol = throw new AssertionError("NoDenotation.owner")
    override def name: Name = BootNameTable.newTermName("<none>")
    override def info: Type = NoType
  }

  object NameFilter {
    final val WordSizeLog = 6
    final val DefinedNamesWords = 16
    final val DefinedNamesSize = DefinedNamesWords << WordSizeLog
    final val DefinedNamesMask = DefinedNamesSize - 1

    type FingerPrint = Array[Long]

    def includeName(bits: FingerPrint, name: Name): Unit = {
      val hash = name.start & DefinedNamesMask
      bits(hash >> 6) |= (1 << hash)
    }

    def includeFingerPrint(bits1: FingerPrint, bits2: FingerPrint): Unit =
      for (i <- 0 until DefinedNamesWords) bits1(i) |= bits2(i)

    def containsName(bits: FingerPrint, name: Name): Boolean = {
      val hash = name.start & DefinedNamesMask
      (bits(hash >> 6) & (1 << hash)) != 0
    }

    def newNameFilter: FingerPrint = new Array[Long](DefinedNamesWords)
  }

  implicit def toFlagSet(denot: SymDenotation): FlagSet = denot.flags

}
