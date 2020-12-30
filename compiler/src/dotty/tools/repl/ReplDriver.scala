package dotty.tools.repl

import java.io.{File => JFile, PrintStream}
import java.nio.charset.StandardCharsets

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.{tpd, untpd}
import dotty.tools.dotc.ast.tpd.TreeOps
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Phases.{unfusedPhases, typerPhase}
import dotty.tools.dotc.core.Denotations.Denotation
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.interactive.Completion
import dotty.tools.dotc.printing.SyntaxHighlighting
import dotty.tools.dotc.reporting.MessageRendering
import dotty.tools.dotc.reporting.{Message, Diagnostic}
import dotty.tools.dotc.util.Spans.Span
import dotty.tools.dotc.util.{SourceFile, SourcePosition}
import dotty.tools.dotc.{CompilationUnit, Driver}
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.io._
import org.jline.reader._

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.util.Using

/** The state of the REPL contains necessary bindings instead of having to have
 *  mutation
 *
 *  The compiler in the REPL needs to do some wrapping in order to compile
 *  valid code. This wrapping occurs when a single `MemberDef` that cannot be
 *  top-level needs to be compiled. In order to do this, we need some unique
 *  identifier for each of these wrappers. That identifier is `objectIndex`.
 *
 *  Free expressions such as `1 + 1` needs to have an assignment in order to be
 *  of use. These expressions are therefore given a identifier on the format
 *  `resX` where `X` starts at 0 and each new expression that needs an
 *  identifier is given the increment of the old identifier. This identifier is
 *  `valIndex`.
 *
 *  @param objectIndex the index of the next wrapper
 *  @param valIndex    the index of next value binding for free expressions
 *  @param imports     a map from object index to the list of user defined imports
 *  @param context     the latest compiler context
 */
case class State(objectIndex: Int,
                 valIndex: Int,
                 imports: Map[Int, List[tpd.Import]],
                 context: Context)

/** Main REPL instance, orchestrating input, compilation and presentation */
class ReplDriver(settings: Array[String],
                 out: PrintStream = Console.out,
                 classLoader: Option[ClassLoader] = None) extends Driver {

  /** Overridden to `false` in order to not have to give sources on the
   *  commandline
   */
  override def sourcesRequired: Boolean = false

  /** Create a fresh and initialized context with IDE mode enabled */
  private def initialCtx = {
    val rootCtx = initCtx.fresh.addMode(Mode.ReadPositions | Mode.Interactive)
    rootCtx.setSetting(rootCtx.settings.YcookComments, true)
    rootCtx.setSetting(rootCtx.settings.YreadComments, true)
    setup(settings, rootCtx) match
      case Some((files, ictx)) =>
        shouldStart = true
        ictx.base.initialize()(using ictx)
        ictx
      case None =>
        shouldStart = false
        rootCtx
  }

  /** the initial, empty state of the REPL session */
  final def initialState: State = State(0, 0, Map.empty, rootCtx)

  /** Reset state of repl to the initial state
   *
   *  This method is responsible for performing an all encompassing reset. As
   *  such, when the user enters `:reset` this method should be called to reset
   *  everything properly
   */
  protected def resetToInitial(): Unit = {
    rootCtx = initialCtx
    if (rootCtx.settings.outputDir.isDefault(using rootCtx))
      rootCtx = rootCtx.fresh
        .setSetting(rootCtx.settings.outputDir, new VirtualDirectory("<REPL compilation output>"))
    compiler = new ReplCompiler
    rendering = new Rendering(classLoader)
  }

  private[repl] def replClassLoader()(using Context) = rendering.classLoader()

  private var rootCtx: Context = _
  private var shouldStart: Boolean = _
  private var compiler: ReplCompiler = _
  private var rendering: Rendering = _

  // initialize the REPL session as part of the constructor so that once `run`
  // is called, we're in business
  resetToInitial()

  override protected def command: CompilerCommand = ReplCommand

  /** Try to run REPL if there is nothing that prevents us doing so.
   *
   *  Possible reason for unsuccessful run are raised flags in CLI like --help or --version
   */
  final def tryRunning = if shouldStart then runUntilQuit()

  /** Run REPL with `state` until `:quit` command found
   *
   *  This method is the main entry point into the REPL. Its effects are not
   *  observable outside of the CLI, for this reason, most helper methods are
   *  `protected final` to facilitate testing.
   */
  final def runUntilQuit(initialState: State = initialState): State = {
    val terminal = new JLineTerminal

    /** Blockingly read a line, getting back a parse result */
    def readLine(state: State): ParseResult = {
      val completer: Completer = { (_, line, candidates) =>
        val comps = completions(line.cursor, line.line, state)
        candidates.addAll(comps.asJava)
      }
      given Context = state.context
      try {
        val line = terminal.readLine(completer)
        ParseResult(line)(state)
      } catch {
        case _: EndOfFileException |
            _: UserInterruptException => // Ctrl+D or Ctrl+C
          Quit
      }
    }

    @tailrec def loop(state: State): State = {
      val res = readLine(state)
      if (res == Quit) state
      else loop(interpret(res)(state))
    }

    try withRedirectedOutput { loop(initialState) }
    finally terminal.close()
  }

  final def run(input: String)(implicit state: State): State = withRedirectedOutput {
    val parsed = ParseResult(input)(state)
    interpret(parsed)
  }

  // TODO: i5069
  final def bind(name: String, value: Any)(implicit state: State): State = state

  // redirecting the output allows us to test `println` in scripted tests
  private def withRedirectedOutput(op: => State): State = {
    val savedOut = System.out
    val savedErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      op
    }
    finally {
      System.setOut(savedOut)
      System.setErr(savedErr)
    }
  }

  private def newRun(state: State) = {
    val run = compiler.newRun(rootCtx.fresh.setReporter(newStoreReporter), state)
    state.copy(context = run.runContext)
  }

  /** Extract possible completions at the index of `cursor` in `expr` */
  protected final def completions(cursor: Int, expr: String, state0: State): List[Candidate] = {
    def makeCandidate(label: String) = {
      new Candidate(
        /* value    = */ label,
        /* displ    = */ label, // displayed value
        /* group    = */ null,  // can be used to group completions together
        /* descr    = */ null,  // TODO use for documentation?
        /* suffix   = */ null,
        /* key      = */ null,
        /* complete = */ false  // if true adds space when completing
      )
    }
    implicit val state = newRun(state0)
    compiler
      .typeCheck(expr, errorsAllowed = true)
      .map { tree =>
        val file = SourceFile.virtual("<completions>", expr, maybeIncomplete = true)
        val unit = CompilationUnit(file)(using state.context)
        unit.tpdTree = tree
        given Context = state.context.fresh.setCompilationUnit(unit)
        val srcPos = SourcePosition(file, Span(cursor))
        val (_, completions) = Completion.completions(srcPos)
        completions.map(_.label).distinct.map(makeCandidate)
      }
      .getOrElse(Nil)
  }

  private def interpret(res: ParseResult)(implicit state: State): State = {
    val newState = res match {
      case parsed: Parsed if parsed.trees.nonEmpty =>
        compile(parsed, state)

      case SyntaxErrors(_, errs, _) =>
        displayErrors(errs)
        state

      case cmd: Command =>
        interpretCommand(cmd)

      case SigKill => // TODO
        state

      case _ => // new line, empty tree
        state
    }
    inContext(newState.context) {
      if (!ctx.settings.XreplDisableDisplay.value)
        out.println()
      newState
    }
  }

  /** Compile `parsed` trees and evolve `state` in accordance */
  private def compile(parsed: Parsed, istate: State): State = {
    def extractNewestWrapper(tree: untpd.Tree): Name = tree match {
      case PackageDef(_, (obj: untpd.ModuleDef) :: Nil) => obj.name.moduleClassName
      case _ => nme.NO_NAME
    }

    def extractTopLevelImports(ctx: Context): List[tpd.Import] =
      unfusedPhases(using ctx).collectFirst { case phase: CollectTopLevelImports => phase.imports }.get

    implicit val state = {
      val state0 = newRun(istate)
      state0.copy(context = state0.context.withSource(parsed.source))
    }
    compiler
      .compile(parsed)
      .fold(
        displayErrors,
        {
          case (unit: CompilationUnit, newState: State) =>
            val newestWrapper = extractNewestWrapper(unit.untpdTree)
            val newImports = extractTopLevelImports(newState.context)
            var allImports = newState.imports
            if (newImports.nonEmpty)
              allImports += (newState.objectIndex -> newImports)
            val newStateWithImports = newState.copy(imports = allImports)

            val warnings = newState.context.reporter
              .removeBufferedMessages(using newState.context)
              .map(rendering.formatError)

            inContext(newState.context) {
              val (updatedState, definitions) =
                if (!ctx.settings.XreplDisableDisplay.value)
                  renderDefinitions(unit.tpdTree, newestWrapper)(newStateWithImports)
                else
                  (newStateWithImports, Seq.empty)

              // output is printed in the order it was put in. warnings should be
              // shown before infos (eg. typedefs) for the same line. column
              // ordering is mostly to make tests deterministic
              implicit val diagnosticOrdering: Ordering[Diagnostic] =
                Ordering[(Int, Int, Int)].on(d => (d.pos.line, -d.level, d.pos.column))

              (definitions ++ warnings)
                .sorted
                .map(_.msg)
                .foreach(out.println)

              updatedState
            }
        }
      )
  }

  private def renderDefinitions(tree: tpd.Tree, newestWrapper: Name)(implicit state: State): (State, Seq[Diagnostic]) = {
    given Context = state.context

    def resAndUnit(denot: Denotation) = {
      import scala.util.{Success, Try}
      val sym = denot.symbol
      val name = sym.name.show
      val hasValidNumber = Try(name.drop(3).toInt) match {
        case Success(num) => num < state.valIndex
        case _ => false
      }
      name.startsWith(str.REPL_RES_PREFIX) && hasValidNumber && sym.info == defn.UnitType
    }

    def extractAndFormatMembers(symbol: Symbol): (State, Seq[Diagnostic]) = if (tree.symbol.info.exists) {
      val info = symbol.info
      val defs =
        info.bounds.hi.finalResultType
          .membersBasedOnFlags(required = Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
          .filterNot { denot =>
            defn.topClasses.contains(denot.symbol.owner) || denot.symbol.isConstructor
          }

      val vals =
        info.fields
          .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
          .filter(_.symbol.name.is(SimpleNameKind))

      val typeAliases =
        info.bounds.hi.typeMembers.filter(_.symbol.info.isTypeAlias)

      val formattedMembers =
        typeAliases.map(rendering.renderTypeAlias) ++
        defs.map(rendering.renderMethod) ++
        vals.flatMap(rendering.renderVal)

      val diagnostics = if formattedMembers.isEmpty then rendering.forceModule(symbol) else formattedMembers

      (state.copy(valIndex = state.valIndex - vals.count(resAndUnit)), diagnostics)
    }
    else (state, Seq.empty)

    def isSyntheticCompanion(sym: Symbol) =
      sym.is(Module) && sym.is(Synthetic)

    def typeDefs(sym: Symbol): Seq[Diagnostic] = sym.info.memberClasses
      .collect {
        case x if !isSyntheticCompanion(x.symbol) && !x.symbol.name.isReplWrapperName =>
          rendering.renderTypeDef(x)
      }

    atPhase(typerPhase.next) {
      // Display members of wrapped module:
      tree.symbol.info.memberClasses
        .find(_.symbol.name == newestWrapper.moduleClassName)
        .map { wrapperModule =>
          val formattedTypeDefs = typeDefs(wrapperModule.symbol)
          val (newState, formattedMembers) = extractAndFormatMembers(wrapperModule.symbol)
          val highlighted = (formattedTypeDefs ++ formattedMembers)
            .map(d => new Diagnostic(d.msg.mapMsg(SyntaxHighlighting.highlight), d.pos, d.level))
          (newState, highlighted)
        }
        .getOrElse {
          // user defined a trait/class/object, so no module needed
          (state, Seq.empty)
        }
    }
  }

  /** Interpret `cmd` to action and propagate potentially new `state` */
  private def interpretCommand(cmd: Command)(implicit state: State): State = cmd match {
    case UnknownCommand(cmd) =>
      out.println(s"""Unknown command: "$cmd", run ":help" for a list of commands""")
      state

    case AmbiguousCommand(cmd, matching) =>
      out.println(s""""$cmd" matches ${matching.mkString(", ")}. Try typing a few more characters. Run ":help" for a list of commands""")
      state

    case Help =>
      out.println(Help.text)
      state

    case Reset =>
      resetToInitial()
      initialState

    case Imports =>
      for {
        objectIndex <- 1 to state.objectIndex
        imp <- state.imports.getOrElse(objectIndex, Nil)
      } out.println(imp.show(using state.context))
      state

    case Load(path) =>
      val file = new JFile(path)
      if (file.exists) {
        val contents = Using(scala.io.Source.fromFile(file, StandardCharsets.UTF_8.name))(_.mkString).get
        run(contents)
      }
      else {
        out.println(s"""Couldn't find file "${file.getCanonicalPath}"""")
        state
      }

    case JavapOf(line) =>
      given DisassemblerRepl = DisassemblerRepl(this, state)
      val opts = JavapOptions.parse(ReplStrings.words(line))
      disassemble(Javap, opts)
      state

    case TypeOf(expr) =>
      expr match {
        case "" => out.println(s":type <expression>")
        case _  =>
          compiler.typeOf(expr)(newRun(state)).fold(
            displayErrors,
            res => out.println(res)  // result has some highlights
          )
      }
      state

    case DocOf(expr) =>
      expr match {
        case "" => out.println(s":doc <expression>")
        case _  =>
          compiler.docOf(expr)(newRun(state)).fold(
            displayErrors,
            res => out.println(res)
          )
      }
      state

    case Quit =>
      // end of the world!
      state
  }

  private def disassemble(tool: Disassembler, opts: DisassemblerOptions)(using DisassemblerRepl): Unit = {
    if opts.targets.isEmpty || opts.flags.contains("-help") then
      out.println(tool.helpText)
    else
      import DisResult._
      tool(opts).foreach {
        case DisSuccess(target, results) =>
          val filter = tool.outputFilter(target, opts)
          out.println(filter(results))
        case DisError(err) =>
          out.println(err)
      }
  }

  /** shows all errors nicely formatted */
  private def displayErrors(errs: Seq[Diagnostic])(implicit state: State): State = {
    errs.map(rendering.formatError).map(_.msg).foreach(out.println)
    state
  }

  extension (sym: Symbol)(using Context)
    // borrowed from ExtractDependencies#recordDependency and adapted to handle @targetName
    def binaryClassName: String =
      val builder = new StringBuilder
      val pkg = sym.enclosingPackageClass
      if !pkg.isEffectiveRoot then
        builder.append(pkg.fullName.mangledString)
        builder.append(".")
      import dotty.tools.dotc.core.NameKinds._
      val flatName = /*sym.flatName*/ sym.maybeOwner.fullNameSeparated(FlatName, FlatName, sym.targetName)
      val clsFlatName = if sym.is(JavaDefined) then flatName.stripModuleClassSuffix else flatName
      builder.append(clsFlatName.mangledString)
      builder.toString

  def disassemblyTargetsLastWrapper(state: State): List[String] =
    implicit val newstate = newRun(state)
    given Context = newstate.context

    def isSyntheticCompanion(sym: Symbol) =
      sym.is(Module) && sym.is(Synthetic)

    def typeDefs(sym: Symbol): List[String] =
      sym.info.memberClasses.collect {
        case x if !isSyntheticCompanion(x.symbol) && !x.symbol.name.isReplWrapperName =>
          x.symbol.binaryClassName
      }.toList

    def hasMembers(sym: Symbol): Boolean =
      val info = sym.info
      def defs =
        info.bounds.hi.finalResultType
          .membersBasedOnFlags(required = Method, excluded = Accessor | ParamAccessor | Synthetic | Private)
          .filterNot { denot =>
            defn.topClasses.contains(denot.symbol.owner) || denot.symbol.isConstructor
          }
      def vals =
        info.fields
          .filterNot(_.symbol.isOneOf(ParamAccessor | Private | Synthetic | Artifact | Module))
          .filter(_.symbol.name.is(SimpleNameKind))
      vals.nonEmpty || defs.nonEmpty

    val lastWrapper = s"${str.REPL_SESSION_LINE}${state.objectIndex}"
    val wrapperModuleClass = List(lastWrapper + "$")

    compiler.typeCheck(lastWrapper).map { tree =>
      tree.rhs match
        case Block(id :: Nil, _) =>
          val sym = id.tpe.typeSymbol
          typeDefs(sym) ++ (if hasMembers(sym) then wrapperModuleClass else Nil)
    }.toOption
    .filterNot(_.isEmpty)
    .getOrElse(wrapperModuleClass)
  end disassemblyTargetsLastWrapper

  /** Is `name` a type symbol in REPL scope?
   *  Returns Some containing its binary class name if so, otherwise None
   */
  def binaryClassOfType(name: String)(implicit state0: State): Option[String] =
    implicit val state = newRun(state0)
    given Context = state.context

    val typeName =
      if name.endsWith("$") then s"${name.init}.type"
      else name

    compiler.typeCheck(s"type $$_ = $typeName", errorsAllowed = true).toOption.flatMap { tree =>
      tree.rhs match
        case Block(List(TypeDef(_, x)), _) =>
          val sym = x.tpe.widenDealias.typeSymbol
          Option.when(sym.exists && sym.isClass)(sym.binaryClassName)
    }
  end binaryClassOfType

  /** Is `name` a symbol in some enclosing class scope?
   *  Returns Some containing its binary class name if so, otherwise None
   */
  def binaryClassOfTerm(name: String)(implicit state0: State): Option[String] =
    implicit val state = newRun(state0)
    given Context = state.context

    def extractSymbol(tree: tpd.Tree): Symbol = tree match
      case tpd.closureDef(defdef) => defdef.rhs.symbol
      case _ => tree.symbol

    compiler.typeCheck(s"val $$_ = $name").toOption.flatMap { tree =>
      tree.rhs match
        case Block((valdef: tpd.ValDef) :: Nil, _) =>
          val sym = extractSymbol(valdef.rhs)
          val encl =
            if sym.is(ModuleVal) then sym.moduleClass
            else sym.lexicallyEnclosingClass
          Option.when(encl.exists)(encl.binaryClassName)
    }
  end binaryClassOfTerm
}
