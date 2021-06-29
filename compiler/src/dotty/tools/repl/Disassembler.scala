package dotty.tools
package repl

import scala.annotation.internal.sharable
import scala.util.{Failure, Success, Try}
import scala.util.matching.Regex

import dotc.core.StdNames.*
import DisResult.*

enum DisResult:
  case DisError(message: String)
  case DisSuccess(target: String, output: String)

abstract class Disassembler:
  import Disassembler.*

  def apply(opts: DisassemblerOptions)(using DisassemblerRepl): List[DisResult]
  def helps: List[(String, String)]

  def helpText: String = helps.map((name, help) => f"${name}%-12.12s${help}%s%n").mkString

  def filters(target: String, opts: DisassemblerOptions): List[String => String] =
    if opts.filter then filterReplNames :: Nil else Nil

  // Combined chain of filters for post-processing output from the disassembler
  def outputFilter(target: String, opts: DisassemblerOptions): String => String =
    filters(target, opts) match
      case Nil => identity
      case fs => Function.chain(fs)

object Disassembler:
  @sharable private val RsLine = (
    Regex.quote(nme.REPL_PACKAGE.toString) + "[./]"
    + Regex.quote(str.REPL_SESSION_LINE) + raw"\d+" + Regex.quote("$") + "?"
  ).r

  def filterReplNames(in: String): String = RsLine.replaceAllIn(in, "")

  def filteredLines(text: String, pred: String => Boolean): String =
    val bldr = StringBuilder()
    text.linesIterator.foreach(line =>
      if pred(line) then
        bldr.append(line).append('\n')
    )
    bldr.toString

  // e.g. Foo#bar. Foo# yields zero-length member part.
  def splitHashMember(s: String): Option[String] =
    s.lastIndexOf('#') match
      case -1 => None
      case  i => Some(s.drop(i + 1))
end Disassembler

case class DisassemblerRepl(driver: ReplDriver, state: State):
  def classLoader: ClassLoader = driver.replClassLoader()(using state.context)
  def mostRecentEntry: Seq[String] = driver.disassemblyTargetsLastWrapper(state)

final case class DisassemblerOptions(flags: Seq[String], targets: Seq[String], filter: Boolean)

abstract class DisassemblerOptionParser(helps: List[(String, String)]):
  def defaultToolOptions: List[String]

  /** Option args start with "-", except that "-" itself denotes the last REPL result. */
  def parse(args: Seq[String])(using repl: DisassemblerRepl): DisassemblerOptions =
    val (options0, targets0) = args.partition(s => s.startsWith("-") && s.length > 1)
    val (options, filter) =
      val (opts, flag) = toolArgs(options0)
      (if opts.isEmpty then defaultToolOptions else opts, flag)

    // "-" may expand into multiple targets (e.g. if multiple type defs in a single wrapper)
    val targets = targets0.flatMap {
      case "-" => repl.mostRecentEntry
      case s   => Seq(s)
    }
    DisassemblerOptions(options, targets, filter)

  // split tool options from REPL's -filter flag, also take prefixes of flag names
  private def toolArgs(args: Seq[String]): (Seq[String], Boolean) =
    val (opts, rest) = args.flatMap(massage).partition(_ != "-filter")
    (opts, rest.nonEmpty)

  private def massage(arg: String): Seq[String] =
    require(arg.startsWith("-"))
    // arg matches opt "-foo/-f" if prefix of -foo or exactly -f
    val r = """(-[^/]*)(?:/(-.))?""".r

    def maybe(opt: String, s: String): Option[String] = opt match
      // disambiguate by preferring short form
      case r(lf, sf) if s == sf          => Some(sf)
      case r(lf, sf) if lf startsWith s  => Some(lf)
      case _ => None

    def candidates(s: String) = helps.map(h => maybe(h._1, s)).flatten

    // one candidate or one single-char candidate
    def uniqueOf(maybes: Seq[String]) =
      def single(s: String) = s.length == 2
      if maybes.length == 1 then maybes
      else if maybes.count(single) == 1 then maybes.filter(single)
      else Nil

    // each optchar must decode to exactly one option
    def unpacked(s: String): Try[Seq[String]] =
      val ones = s.drop(1).map(c =>
        val maybes = uniqueOf(candidates(s"-$c"))
        if maybes.length == 1 then Some(maybes.head) else None
      )
      Try(ones) filter (_ forall (_.isDefined)) map (_.flatten)

    val res = uniqueOf(candidates(arg))
    if res.nonEmpty then res
    else unpacked(arg).getOrElse(Seq("-help")) // or else someone needs help
  end massage
end DisassemblerOptionParser

/** The task or tool provider. */
abstract class DisassemblyTool:
  import DisassemblyTool.*
  def apply(options: Seq[String])(inputs: Seq[Input]): List[DisResult]

object DisassemblyTool:
  case class Input(target: String, actual: String, data: Try[Array[Byte]])

/** Responsible for:
 *   - gathering the Array[Bytes] containing the data to be decompiled
 */
class DisassemblyClass(loader: ClassLoader)(using repl: DisassemblerRepl):
  import DisassemblyClass.*
  import DisassemblyTool.*
  import ClassLoaderOps.*
  import java.io.FileNotFoundException
  import dotty.tools.io.File

  /** Associate the requested path with a possibly failed or empty array of bytes. */
  def bytes(path: String): Input =
    bytesFor(path) match
      case Success((actual, bytes)) => Input(path, actual, Success(bytes))
      case f: Failure[_]            => Input(path, path, Failure(f.exception))

  /** Find bytes. Handle "Foo#bar" (by ignoring member), "#bar" (by taking "bar").
   *  @return the path to use for filtering, and the byte array
   */
  private def bytesFor(path: String) =
    Try {
      path match
        case HashSplit(prefix, _) if prefix != null => prefix
        case HashSplit(_, member) if member != null => member
        case s                                      => s
    }.flatMap(findBytes)

  // data paired with actual path where it was found
  private def findBytes(path: String) = tryFile(path) orElse tryClass(path)

  /** Assume the string is a path and try to find the classfile it represents. */
  private def tryFile(path: String): Try[(String, Array[Byte])] =
    Try(File(path.asClassResource))
      .filter(_.exists)
      .map(f => (path, f.toByteArray()))

  /** Assume the string is a fully qualified class name and try to
   *  find the class object it represents.
   *  There are other symbols of interest, too:
   *  - a definition that is wrapped in an enclosing class
   *  - a synthetic that is not in scope but its associated class is
   */
  private def tryClass(path: String): Try[(String, Array[Byte])] =
    given State = repl.state

    def loadable(name: String) = loader.resourceable(name)

    // if path has an interior dollar, take it as a synthetic
    // if the prefix up to the dollar is a symbol in scope,
    // result is the translated prefix + suffix
    def desynthesize(s: String): Option[String] =
      val i = s.indexOf('$')
      if 0 until s.length - 1 contains i then
        val name = s.substring(0, i)
        val sufx = s.substring(i)

        def loadableOrNone(strip: Boolean) =
          def suffix(strip: Boolean)(x: String) =
            (if strip && x.endsWith("$") then x.init else x) + sufx
          repl.driver.binaryClassOfType(name)
            .map(suffix(strip)(_))
            .filter(loadable)

        // try loading translated+suffix
        // some synthetics lack a dollar, (e.g., suffix = delayedInit$body)
        // so as a hack, if prefix$$suffix fails, also try prefix$suffix
        loadableOrNone(strip = false)
          .orElse(loadableOrNone(strip = true))
      else
        None
    end desynthesize

    // if repl, translate the name to something replish
    // (for translate, would be nicer to get the sym and ask .isClass,
    // instead of translatePath and then asking did I get a class back)

    def scopedClass(name: String): Option[String] = repl.driver.binaryClassOfType(name).filter(loadable)
    def enclosingClass(name: String): Option[String] = repl.driver.binaryClassOfTerm(name).filter(loadable)
    def qualifiedName(name: String): Option[String] = Some(name).filter(_.contains('.')).filter(loadable)

    val p = path.asClassName   // scrub any suffix
    val className =
      qualifiedName(p)
      .orElse(scopedClass(p))
      .orElse(enclosingClass(p))
      .orElse(desynthesize(p))
      .getOrElse(p)

    val classBytes = loader.classBytes(className)

    if classBytes.isEmpty then
      Failure(FileNotFoundException(s"Could not find class bytes for '$path'"))
    else
      Success(className, classBytes)

  end tryClass

object DisassemblyClass:
  private final val classSuffix = ".class"

  /** Match foo#bar, both groups are optional (may be null). */
  @sharable private val HashSplit = "([^#]+)?(?:#(.+)?)?".r

  // We enjoy flexibility in specifying either a fully-qualified class name com.acme.Widget
  // or a resource path com/acme/Widget.class; but not widget.out
  extension (s: String)
    def asClassName = s.stripSuffix(classSuffix).replace('/', '.')
    def asClassResource = if s.endsWith(classSuffix) then s else s.replace('.', '/') + classSuffix

  extension (cl: ClassLoader)
    /** Would classBytes succeed with a nonempty array */
    def resourceable(className: String): Boolean =
      cl.getResource(className.asClassResource) != null
end DisassemblyClass
