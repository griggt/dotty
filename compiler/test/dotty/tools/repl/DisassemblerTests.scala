package dotty.tools.repl

import org.junit.Test
import org.junit.Assert.{assertEquals, fail}
import dotty.tools.dotc.core.StdNames.{nme, str}
import scala.annotation.tailrec

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
    @tailrec def loop(input: Iterator[String], expected: List[String]): Unit =
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
