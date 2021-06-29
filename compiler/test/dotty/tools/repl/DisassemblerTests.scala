package dotty.tools.repl

import org.junit.Test
import org.junit.Assert.{assertEquals, fail}
import dotty.tools.dotc.core.StdNames.{nme, str}

// Test target resolution
class DisassemblyTargetTests extends ReplTest:
  import DisassemblyTool.Input

  private def eval(code: String): State =
    fromInitialState { implicit s => run(code) }

  private def line(n: Int): String = s"${nme.REPL_PACKAGE}.${str.REPL_SESSION_LINE}$n"
  private def line(n: Int, rest: String): String = line(n) + "$" + rest

  private def assertTargets(expected: Seq[String], targets: Seq[String])(implicit s: State): Unit =
    given repl: DisassemblerRepl = DisassemblerRepl(this, s)
    val clazz = DisassemblyClass(repl.classLoader)
    val targetNames = targets.flatMap(t => if t == "-" then repl.mostRecentEntry else Seq(t))
    assertEquals(expected, targetNames.map(clazz.bytes(_).actual))

  private def assertTarget(expected: String, target: String)(implicit s: State): Unit =
    assertTargets(expected :: Nil, target :: Nil)

  @Test def targetOfStdlib =
    fromInitialState { implicit s =>
      assertTarget("scala.collection.immutable.List", "List")
      assertTarget("scala.collection.immutable.List$", "List$")
      assertTarget("scala.collection.immutable.List$", "List.type")
      assertTarget("scala.collection.immutable.List", "List#tail")
    }

  @Test def targetOfJavaStdlib =
    fromInitialState { implicit s =>
      assertTarget("java.lang.String", "String#substring")
      assertTarget("java.lang.Character$UnicodeBlock", "Character.UnicodeBlock")
    }

  @Test def targetOfEmptyClass =
    eval("class C").andThen { implicit s =>
      assertTarget(line(1, "C"), "C")
    }

  @Test def targetOfEnum =
    eval(
      """enum Color {
        |  case Red, Green, Blue
        |}
      """.stripMargin
    ).andThen { implicit s =>
      assertTarget(line(1, "Color"), "Color")
      assertTarget(line(1, "Color$"), "Color$")
      assertTarget(line(1, "Color$"), "Color.type")
      assertTarget(line(1, "Color$"), "Color.Green")
    }

  @Test def targetOfClassInsideObject =
    eval(
      """object Hello {
        |  class C {
        |    def hello() = println("hi")
        |  }
        |}
      """.stripMargin
    ).andThen { implicit s =>
      assertTarget(line(1, "Hello$"), "Hello")
      assertTarget(line(1, "Hello$C"), "Hello.C")
      //assertTarget(line(1, "Hello$C"), "Hello$C")  // XXX doesn't work
    }

  @Test def targetOfClassInsideClass =
    eval(
      """class Greeting {
        |  class D {
        |    def howdy() = println("howdy")
        |  }
        |}
      """.stripMargin
    ).andThen { implicit s =>
      assertTarget(line(1, "Greeting"), "Greeting")
      assertTarget(line(1, "Greeting$D"), "Greeting$D")
      //assertTarget(line(1, "Greeting$D"), "Greeting#D")  // XXX fails, triggers filtering instead of inner class selection
    }

  @Test def targetOfTypeConstructor =
    eval(
      """class Id[A] {
        |  def me(in: A): A = in
        |}
      """.stripMargin
    ).andThen { implicit s =>
      assertTarget(line(1, "Id"), "Id")
      assertTarget(line(1, "Id"), "Id[_]")
      assertTarget(line(1, "Id"), "Id#me")
    }

  @Test def targetOfTypeAlias =
    eval(
      """class IntAdder {
        |  def addOne(x: Int) = x + 1
        |}
        |type IA = IntAdder
      """.stripMargin
    ).andThen { implicit s =>
      assertTarget(line(1, "IntAdder"), "IA")
      assertTarget(line(1, "IntAdder"), "IA#addOne")
    }

  @Test def targetOfTargetNameClass =
    eval(
      """import scala.annotation.targetName
        |@targetName("Target") class Defined {
        |  def self: Defined = this
        |}
      """.stripMargin
    ).andThen { implicit s =>
      assertTarget(line(1, "Target"), "Defined")
      assertTarget("Target", "Target")  // fall back to verbatim requested target
    }

  @Test def targetOfSimpleVal =
    eval("val x = 42").andThen { implicit s =>
      assertTarget(line(1, ""), "x")
    }

  @Test def targetOfNullaryMethod =
    eval("def nonRandom() = 42").andThen { implicit s =>
      assertTarget(line(1, ""), "nonRandom()")
    }

  @Test def targetOfJavaStaticVal =
    fromInitialState { implicit s =>
      assertTarget("java.time.DayOfWeek", "java.time.DayOfWeek.MONDAY")
    }

  @Test def targetOfLast =
    eval("class C").andThen { implicit s => assertTarget(line(1, "C"), "-") }
    eval("object X").andThen { implicit s => assertTarget(line(1, "X$"), "-") }
    eval("val t = 0").andThen { implicit s => assertTarget(line(1, ""), "-") }
    eval("def f = 10").andThen { implicit s => assertTarget(line(1, ""), "-") }
    eval(
      """class C
        |class D
      """.stripMargin
    ).andThen { implicit s =>
      assertTargets(List(line(1, "C"), line(1, "D")), Seq("-"))
    }
    eval(
      """import scala.annotation.targetName
        |@targetName("Target") class Defined
      """.stripMargin
    ).andThen { implicit s =>
      assertTarget(line(1, "Target"), "-")
    }
end DisassemblyTargetTests

abstract class DisassemblerTest extends ReplTest:
  def packageSeparator: String
  def line(n: Int): String = s"${nme.REPL_PACKAGE}${packageSeparator}${str.REPL_SESSION_LINE}$n"
  def line(n: Int, rest: String): String = line(n) + "$" + rest

  def eval(code: String): State =
    val state = fromInitialState { implicit s => run(code) }
    val _ = storedOutput()  // discard output
    state

  def assertDisassemblyIncludes(line: String, output: String = null): Unit =
    val out = if output eq null then storedOutput() else output
    assert(out.linesIterator.exists(_.contains(line)),
      s"disassembly did not contain `$line`\nDisassembly was:\n$out")

  // NB: supplied expected lines must occur in the same order in the output
  def assertDisassemblyIncludes(lines: List[String]): Unit =
    val out = storedOutput()
    @annotation.tailrec
    def loop(input: Iterator[String], expected: List[String]): Unit =
      expected match
        case Nil =>
        case x :: xs =>
          val it = input.dropWhile(!_.contains(x))
          assert(it.hasNext, s"disassembly did not contain `$x`\nDisassembly was:\n$out")
          loop(it.drop(1), xs)
    loop(out.linesIterator, lines)

  def assertDisassemblyExcludes(line: String, output: String = null): Unit =
    val out = if output eq null then storedOutput() else output
    assert(!out.linesIterator.exists(_.contains(line)),
      s"disassembly unexpectedly contained `$line`")
end DisassemblerTest

class JavapTests extends DisassemblerTest:
  val packageSeparator = "."

  @Test def `simple end-to-end` =
    eval("class Foo1").andThen { implicit state =>
      run(":javap -c Foo1")
      assertDisassemblyIncludes(s"public class ${line(1, "Foo1")} {")
    }

  @Test def `multiple classes in prev entry` =
    eval {
      """class Foo2
        |trait Bar2
        |""".stripMargin
    } andThen { implicit state =>
      run(":javap -c -")
      assertDisassemblyIncludes(List(
        s"public class ${line(1, "Foo2")} {",
        s"public interface ${line(1, "Bar2")} {",
      ))
    }

  @Test def `private selected method` =
    eval {
      """class Baz1:
        |  private def one = 1
        |  private def two = 2
        |""".stripMargin
    } andThen { implicit state =>
      run(":javap -p -c Baz1#one")
      val out = storedOutput()
      assertDisassemblyIncludes("private int one();", out)
      assertDisassemblyExcludes("private int two();", out)
    }

  @Test def `java.lang.String signatures` =
    fromInitialState { implicit state =>
      run(":javap -s java.lang.String")
      val out = storedOutput()
      assertDisassemblyIncludes("public static java.lang.String format(java.lang.String, java.lang.Object...);", out)
      assertDisassemblyIncludes("public static java.lang.String join(java.lang.CharSequence, java.lang.Iterable<? extends java.lang.CharSequence>);", out)
      assertDisassemblyIncludes("public java.lang.String concat(java.lang.String);", out)
      assertDisassemblyIncludes("public java.lang.String trim();", out)
    }
end JavapTests

// Test option parsing
class JavapOptionTests extends ReplTest:
  private def assertFlags(expected: Seq[String], input: Seq[String])(implicit s: State): Unit =
    given DisassemblerRepl = DisassemblerRepl(this, s)
    val opts = JavapOptions.parse(input)
    assertEquals(expected, opts.flags)

  private def assertFilter(expected: Boolean, input: Seq[String])(implicit s: State): Unit =
    given DisassemblerRepl = DisassemblerRepl(this, s)
    val opts = JavapOptions.parse(input)
    assertEquals(expected, opts.filter)

  @Test def optionFilter =
    fromInitialState { implicit s =>
      assertFilter(false, Nil)
      assertFilter(false, Seq("-p", "-v"))
      assertFilter(false, Seq("-", "filter"))
      assertFilter(true, Seq("-filter"))
      assertFilter(true, Seq("-filt"))
      assertFilter(true, Seq("-f", "-p", "-v"))
      assertFilter(true, Seq("-p", "-f", "-v"))
      assertFilter(true, Seq("-v", "-p", "-f"))
    }

  @Test def flags =
    fromInitialState { implicit s =>
      assertFlags(Seq("-protected", "-verbose"), Nil)   // default options
      assertFlags(Seq("-private"), Seq("-private", "target"))
      assertFlags(Seq("-private"), Seq("-priv", "-filt", "target"))
      assertFlags(Seq("-p"), Seq("-p", "target"))
    }

  // unrecognized flags should result in -help
  @Test def flagsUnknown =
    fromInitialState { implicit s =>
      assertFlags(Seq("-help"), Seq("-unknown", "target"))
    }
end JavapOptionTests

// Test output filters
class JavapFilterTests:
  // test -sysinfo disassembly
  private val listSysinfo =
    """  Size 51190 bytes
      |  MD5 checksum fa1f9a810f5fff1bac4c3d1ae2051ab5
      |  Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |  public scala.collection.LinearSeq drop(int);
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |  public java.lang.Object drop(int);
      |  public java.lang.Object sorted(scala.math.Ordering);
      |}
    """.stripMargin

  @Test
  def `select drop from listSysinfo`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |  public java.lang.Object drop(int);
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listSysinfo))

  // test -l disassembly
  private val listL =
    """Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |    LineNumberTable:
      |      line 648: 4
      |
      |  public scala.collection.LinearSeq drop(int);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1     n   I
      |
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1     p   Lscala/Function1;
      |
      |  public java.lang.Object drop(int);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1     n   I
      |
      |  public java.lang.Object sorted(scala.math.Ordering);
      |    LineNumberTable:
      |      line 79: 0
      |    LocalVariableTable:
      |      Start  Length  Slot  Name   Signature
      |          0       6     0  this   Lscala/collection/immutable/List;
      |          0       6     1   ord   Lscala/math/Ordering;
      |}
    """.stripMargin

  @Test
  def `select drop from listL`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |    LineNumberTable:
         |      line 79: 0
         |    LocalVariableTable:
         |      Start  Length  Slot  Name   Signature
         |          0       6     0  this   Lscala/collection/immutable/List;
         |          0       6     1     n   I
         |  public java.lang.Object drop(int);
         |    LineNumberTable:
         |      line 79: 0
         |    LocalVariableTable:
         |      Start  Length  Slot  Name   Signature
         |          0       6     0  this   Lscala/collection/immutable/List;
         |          0       6     1     n   I
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listL))

  // test -v disassembly
  private val listV =
    """|
    """.stripMargin

  // test -s disassembly
  private val listS =
    """Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |    descriptor: (Lscala/collection/SeqOps;)Lscala/collection/SeqOps;
      |
      |  public scala.collection.LinearSeq drop(int);
      |    descriptor: (I)Lscala/collection/LinearSeq;
      |
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |    descriptor: (Lscala/Function1;)Lscala/collection/LinearSeq;
      |
      |  public java.lang.Object drop(int);
      |    descriptor: (I)Ljava/lang/Object;
      |
      |  public java.lang.Object sorted(scala.math.Ordering);
      |    descriptor: (Lscala/math/Ordering;)Ljava/lang/Object;
      |}
    """.stripMargin

  @Test
  def `select drop from listS`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |    descriptor: (I)Lscala/collection/LinearSeq;
         |  public java.lang.Object drop(int);
         |    descriptor: (I)Ljava/lang/Object;
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listS))

  private val listC =
    """Compiled from "List.scala"
      |public abstract class scala.collection.immutable.List<A> extends scala.collection.immutable.AbstractSeq<A> implements scala.collection.immutable.LinearSeq<A>, scala.collection.StrictOptimizedLinearSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.immutable.StrictOptimizedSeqOps<A, scala.collection.immutable.List, scala.collection.immutable.List<A>>, scala.collection.generic.DefaultSerializable {
      |  public static scala.collection.SeqOps unapplySeq(scala.collection.SeqOps);
      |    Code:
      |       0: getstatic     #43                 // Field scala/collection/immutable/List$.MODULE$:Lscala/collection/immutable/List$;
      |       3: pop
      |       4: aload_0
      |       5: areturn
      |
      |  public scala.collection.LinearSeq drop(int);
      |    Code:
      |       0: aload_0
      |       1: iload_1
      |       2: invokestatic  #223                // InterfaceMethod scala/collection/StrictOptimizedLinearSeqOps.drop$:(Lscala/collection/StrictOptimizedLinearSeqOps;I)Lscala/collection/LinearSeq;
      |       5: areturn
      |
      |  public scala.collection.LinearSeq dropWhile(scala.Function1);
      |    Code:
      |       0: aload_0
      |       1: aload_1
      |       2: invokestatic  #230                // InterfaceMethod scala/collection/StrictOptimizedLinearSeqOps.dropWhile$:(Lscala/collection/StrictOptimizedLinearSeqOps;Lscala/Function1;)Lscala/collection/LinearSeq;
      |       5: areturn
      |
      |  public java.lang.Object drop(int);
      |    Code:
      |       0: aload_0
      |       1: iload_1
      |       2: invokevirtual #792                // Method drop:(I)Lscala/collection/LinearSeq;
      |       5: areturn
      |
      |  public java.lang.Object sorted(scala.math.Ordering);
      |    Code:
      |       0: aload_0
      |       1: aload_1
      |       2: invokestatic  #210                // InterfaceMethod scala/collection/immutable/StrictOptimizedSeqOps.sorted$:(Lscala/collection/immutable/StrictOptimizedSeqOps;Lscala/math/Ordering;)Ljava/lang/Object;
      |       5: areturn
      |}
    """.stripMargin

  @Test
  def `select drop from List disassembly`: Unit =
    assertEquals(
      """|  public scala.collection.LinearSeq drop(int);
         |    Code:
         |       0: aload_0
         |       1: iload_1
         |       2: invokestatic  #223                // InterfaceMethod scala/collection/StrictOptimizedLinearSeqOps.drop$:(Lscala/collection/StrictOptimizedLinearSeqOps;I)Lscala/collection/LinearSeq;
         |       5: areturn
         |  public java.lang.Object drop(int);
         |    Code:
         |       0: aload_0
         |       1: iload_1
         |       2: invokevirtual #792                // Method drop:(I)Lscala/collection/LinearSeq;
         |       5: areturn
         |""".stripMargin,
      Javap.filterSelection("List#drop")(listC))

  @Test
  def `select last method from disassembly`: Unit =
    assertEquals(
      """|  public java.lang.Object sorted(scala.math.Ordering);
         |    Code:
         |       0: aload_0
         |       1: aload_1
         |       2: invokestatic  #210                // InterfaceMethod scala/collection/immutable/StrictOptimizedSeqOps.sorted$:(Lscala/collection/immutable/StrictOptimizedSeqOps;Lscala/math/Ordering;)Ljava/lang/Object;
         |       5: areturn
         |""".stripMargin,
      Javap.filterSelection("List#sorted")(listC))
end JavapFilterTests
