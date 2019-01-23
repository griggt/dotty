package scala.tasty.reflect

trait FlagsOps extends Core {

  trait FlagsAPI {
    /** Is the given flag set a subset of this flag sets */
    def is(flagSet: Flags): Boolean
    /** Union of the two flag sets */
    def |(flagSet: Flags): Flags
    /** Intersection of the two flag sets */
    def &(flagSet: Flags): Flags
  }
  implicit def FlagsDeco(flagSet: Flags): FlagsAPI

  val Flags: FlagsModule
  abstract class FlagsModule {

    /** Is this symbol `private` */
    def Private: Flags

    /** Is this symbol `protected` */
    def Protected: Flags

    /** Is this symbol `abstract` */
    def Abstract: Flags

    /** Is this symbol `final` */
    def Final: Flags

    /** Is this symbol `sealed` */
    def Sealed: Flags

    /** Is this symbol `case` */
    def Case: Flags

    /** Is this symbol `implicit` */
    def Implicit: Flags

    /** Is this symbol `erased` */
    def Erased: Flags

    /** Is this symbol `lazy` */
    def Lazy: Flags

    /** Is this symbol `override` */
    def Override: Flags

    /** Is this symbol `inline` */
    def Inline: Flags

    /** Is this symbol markes as a macro. An inline method containing toplevel splices */
    def Macro: Flags

    /** Is this symbol marked as static. Mapped to static Java member */
    def Static: Flags

    /** Is this symbol defined in a Java class */
    def JavaDefined: Flags

    /** Is this symbol an object or its class (used for a ValDef or a ClassDef extends Modifier respectively) */
    def Object: Flags

    /** Is this symbol a trait */
    def Trait: Flags

    /** Is this symbol local? Used in conjunction with private/private[Type] to mean private[this] extends Modifier proctected[this] */
    def Local: Flags

    /** Was this symbol generated by Scala compiler */
    def Synthetic: Flags

    /** Is this symbol to be tagged Java Synthetic */
    def Artifact: Flags

    /** Is this symbol a `var` (when used on a ValDef) */
    def Mutable: Flags

    /** Is this symbol a getter or a setter */
    def FieldAccessor: Flags

    /** Is this symbol a getter for case class parameter */
    def CaseAcessor: Flags

    /** Is this symbol a type parameter marked as covariant `+` */
    def Covariant: Flags

    /** Is this symbol a type parameter marked as contravariant `-` */
    def Contravariant: Flags

    /** Was this symbol imported from Scala2.x */
    def Scala2X: Flags

    /** Is this symbol a method with default parameters */
    def DefaultParameterized: Flags

    /** Is this symbol member that is assumed to be stable */
    def Stable: Flags

    /** Is this symbol a parameter */
    def Param: Flags

    /** Is this symbol a parameter accessor */
    def ParamAccessor: Flags
  }

}
