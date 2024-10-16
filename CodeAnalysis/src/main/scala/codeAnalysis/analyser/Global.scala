package codeAnalysis.analyser

import codeAnalysis.analyser.FractionPart.FractionPart
import codeAnalysis.util.Extensions.DoubleExtension

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.DefinedPosition
import scala.tools.nsc.reporters.Reporter
import scala.tools.nsc.{Settings, interactive}

/**
 * The global instance, which provides a set of types and operations associated with a compiler run.
 *
 * @see `scala.reflect.api.Universe`
 */
class Global(settings: Settings, reporter: Reporter) extends interactive.Global(settings, reporter) {

  // Code based on Scapegoat (https://github.com/sksamuel/scapegoat)
  trait ScapegoatTraverser extends Traverser {

    private val SuppressWarnings = typeOf[SuppressWarnings]

    @scala.annotation.tailrec
    private def inspectionClass(klass: Class[_]): Class[_] =
      Option(klass.getEnclosingClass) match {
        case None => klass
        case Some(k) => inspectionClass(k)
      }

    private def isThisDisabled(an: AnnotationInfo): Boolean = {
      val cls = inspectionClass(getClass)
      val names = Set("all", cls.getSimpleName, cls.getCanonicalName).map(_.toLowerCase)
      val suppressedNames: Seq[String] = an.javaArgs.values.headOption.toSeq.flatMap {
        case ArrayAnnotArg(args) =>
          args.collect {
            case LiteralAnnotArg(Constant(suppressedName: String)) =>
              suppressedName.toLowerCase
          }
        case _ =>
          Seq.empty[String]
      }
      names.intersect(suppressedNames.toSet).nonEmpty
    }

    private def isSkipAnnotation(an: AnnotationInfo) =
    // Workaround for #222: we can't use typeOf[Safe] here it requires Scapegoat to be on the
    // compile classpath.
      an.tree.tpe =:= SuppressWarnings || an.tree.tpe.erasure.toString == "com.sksamuel.scapegoat.Safe"

    private def isSuppressed(symbol: Symbol) =
      symbol != null &&
        symbol.annotations.exists(an => isSkipAnnotation(an) && isThisDisabled(an))

    protected def continue(tree: Tree): Unit = super.traverse(tree)

    protected def inspect(tree: Tree): Unit

    override final def traverse(tree: Tree): Unit = {
      tree match {
        // ignore synthetic methods added
        case DefDef(_, _, _, _, _, _) if tree.symbol.isSynthetic ||
          // MODIFIED: Additional ignored methods
          !tree.symbol.isSourceMethod || tree.symbol.isConstructor || tree.symbol.isAccessor =>
        case _: ImplDef if tree.symbol.isSynthetic => // MODIFIED: Ignore synthetic classes
        case member: MemberDef if isSuppressed(member.symbol) =>
        case block@Block(_, _) if isSuppressed(block.symbol) =>
        case iff@If(_, _, _) if isSuppressed(iff.symbol) =>
        case tri@Try(_, _, _) if isSuppressed(tri.symbol) =>
        case ClassDef(_, _, _, Template(parents, _, _))
          if parents.collect { case x if x.tpe != null => x.tpe.typeSymbol.fullName }.contains("scala.reflect.api.TypeCreator") =>
        case _ if analyzer.hasMacroExpansionAttachment(tree) => //skip macros as per http://bit.ly/2uS8BrU
        case _ => inspect(tree)
      }
    }

    protected def isArray(tree: Tree): Boolean = tree.tpe.typeSymbol.fullName == "scala.Array"

    protected def isTraversable(tree: Tree): Boolean = tree.tpe <:< typeOf[Traversable[Any]]

    protected def isSeq(t: Tree): Boolean = t.tpe <:< typeOf[Seq[Any]]

    protected def isIndexedSeq(t: Tree): Boolean = t.tpe <:< typeOf[IndexedSeq[Any]]

    protected def isSet(t: Tree, allowMutableSet: Boolean = true): Boolean = {
      t.tpe.widen.baseClasses.exists { c =>
        (allowMutableSet && c.fullName == "scala.collection.mutable.Set") || c.fullName == "scala.collection.immutable.Set"
      }
    }

    protected def isList(t: Tree): Boolean = t.tpe <:< typeOf[scala.collection.immutable.List[Any]]

    protected def isMap(tree: Tree): Boolean =
      tree.tpe.baseClasses.exists {
        _.fullName == "scala.collection.Map"
      }
  }

  class ParentTraverser[T](f: Option[T] => PartialFunction[Tree, T]) extends ScapegoatTraverser {

    private val stack: mutable.Stack[T] = mutable.Stack()

    def top(tree: Global#Tree): Option[T] = if (tree == null) None else {
      traverse(tree.asInstanceOf[Tree])
      val result = stack.headOption
      stack.clear()
      result
    }

    override protected def inspect(tree: Tree): Unit = {
      val head = stack.headOption
      val partialFunc = f(head)
      if (partialFunc.isDefinedAt(tree)) {
        stack.push(partialFunc(tree))
        continue(tree)
        if (head.isDefined) stack.pop() // Keep top value for return
      } else {
        continue(tree)
      }
    }
  }

  class FoldTraverser[T](base: T)(f: (T, Tree) => T) extends ScapegoatTraverser {
    private var value: T = base

    def fold(tree: Global#Tree): T = {
      traverse(tree.asInstanceOf[Tree])
      val result = value
      value = base
      result
    }

    override protected def inspect(tree: Tree): Unit = {
      value = f(value, tree)
      continue(tree)
    }
  }

  class SumTraverser(f: PartialFunction[Tree, Int]) extends
    FoldTraverser[Int](0)((value, tree) => value + f.applyOrElse(tree, (_: Tree) => 0)) {
    def sum(tree: Global#Tree): Int = fold(tree)
  }

  class CountTraverser(f: PartialFunction[Tree, Boolean]) extends
    SumTraverser(f.andThen(result => if (result) 1 else 0)) {
    def count(tree: Global#Tree): Int = sum(tree)
  }

  class ScopeFoldTraverser[T](base: T)(f: mutable.Stack[ListBuffer[String]] => (T, Tree) => T) extends ScapegoatTraverser {
    private var value: T = base
    private val scopes = mutable.Stack[ListBuffer[String]]()

    private def enter(): Unit = scopes.push(ListBuffer())

    private def add(name: String): Unit = scopes.top.append(name.trim)

    private def exit(): Unit = scopes.pop()

    def fold(tree: Global#Tree): T = {
      traverse(tree.asInstanceOf[Tree])
      val result = value
      value = base
      result
    }

    override protected def inspect(tree: Tree): Unit = {
      value = f(scopes)(value, tree)
      tree match {
        case _: ImplDef | _: Function | _: Block | _: CaseDef =>
          enter()
          continue(tree)
          exit()
        case DefDef(_, TermName(name), _, _, _, _) =>
          add(name)
          enter()
          continue(tree)
          exit()
        case ValDef(_, TermName(name), _, _) =>
          add(name)
          continue(tree)
        case Bind(name, _) =>
          add(name.toString)
          continue(tree)
        case _ => continue(tree)
      }
    }
  }

  class ScopeSumTraverser(f: mutable.Stack[ListBuffer[String]] => PartialFunction[Tree, Int]) extends
    ScopeFoldTraverser[Int](0)(scopes => (value, tree) => value + f(scopes).applyOrElse(tree, (_: Tree) => 0)) {
    def sum(tree: Global#Tree): Int = fold(tree)
  }

  class ScopeCountTraverser(f: mutable.Stack[ListBuffer[String]] => PartialFunction[Tree, Boolean]) extends
    ScopeSumTraverser(scopes => f(scopes).andThen(result => if (result) 1 else 0)) {
    def count(tree: Global#Tree): Int = sum(tree)
  }

  class FractionTraverser(f: PartialFunction[Tree, FractionPart]) extends ScapegoatTraverser {
    private var numerator, denominator = 0

    def fraction(tree: Global#Tree): Double = {
      traverse(tree.asInstanceOf[Tree])
      val result = numerator.toDouble \ denominator.toDouble
      numerator = 0
      denominator = 0
      result
    }

    override protected def inspect(tree: Tree): Unit = {
      if (f.isDefinedAt(tree)) {
        f(tree) match {
          case FractionPart.Numerator => numerator += 1
          case FractionPart.Denominator => denominator += 1
          case FractionPart.Both => numerator += 1; denominator += 1
          case FractionPart.None =>
        }
      }
      continue(tree)
    }
  }

  class FindTraverser(f: PartialFunction[Tree, Boolean]) extends ScapegoatTraverser {
    private var value: Option[Tree] = None

    def find(tree: Global#Tree): Option[Tree] = {
      traverse(tree.asInstanceOf[Tree])
      val result = value
      value = None
      result
    }

    override protected def inspect(tree: Tree): Unit = {
      if (value.isEmpty) {
        if (f.applyOrElse(tree, (_: Tree) => false)) value = Some(tree) else continue(tree)
      }
    }
  }

  class ExistsTraverser(f: PartialFunction[Tree, Boolean]) extends FindTraverser(f) {
    def exists(tree: Global#Tree): Boolean = find(tree).isDefined
  }

  class ForallTraverser(f: PartialFunction[Tree, Boolean]) extends ExistsTraverser(f.andThen(!_)) {
    def forall(tree: Global#Tree): Boolean = !exists(tree)
  }

  class FilterTraverser(f: PartialFunction[Tree, Boolean]) extends ScapegoatTraverser {
    private var value: ListBuffer[Tree] = ListBuffer()

    def filter(tree: Global#Tree): List[Tree] = {
      traverse(tree.asInstanceOf[Tree])
      val result = value.toList
      value.clear()
      result
    }

    override protected def inspect(tree: Tree): Unit = {
      if (f.applyOrElse(tree, (_: Tree) => false)) value += tree
      continue(tree)
    }
  }

  class CollectTraverser[T](f: PartialFunction[Tree, T]) extends ScapegoatTraverser {
    private var value: ListBuffer[T] = ListBuffer()

    def collect(tree: Global#Tree): List[T] = {
      traverse(tree.asInstanceOf[Tree])
      val result = value.toList
      value.clear()
      result
    }

    override protected def inspect(tree: Tree): Unit = {
      if (f.isDefinedAt(tree)) value += f(tree)
      continue(tree)
    }
  }

  class LinesTraverser(f: PartialFunction[Tree, Boolean]) extends ScapegoatTraverser {
    private val lines: mutable.Set[Int] = mutable.Set()
    private var keepCount: Boolean = false

    def lines(tree: Global#Tree): Int = {
      traverse(tree.asInstanceOf[Tree])
      val result = lines.size
      lines.clear()
      result
    }

    private def addLines(tree: Tree): Unit = tree.pos match {
      case pos: DefinedPosition =>
        lines += pos.source.offsetToLine(pos.start)
        lines += pos.source.offsetToLine(pos.end)
      case _ => // Do nothing
    }

    override protected def inspect(tree: Tree): Unit = if (keepCount) {
      addLines(tree)
      continue(tree)
    } else if (f.applyOrElse(tree, (_: Tree) => false)) {
      addLines(tree)
      keepCount = true
      continue(tree)
      keepCount = false
    } else {
      continue(tree)
    }
  }

  class CyclomaticComplexityTraverser extends ScapegoatTraverser {
    private var value = 1

    def complexity(tree: Tree): Int = {
      traverse(tree)
      val result = value
      value = 1
      result
    }

    override protected def inspect(tree: Tree): Unit = tree match {
      case _: If | _: CaseDef => // While and do while are translated to use if statements and recursion
        value += 1
        continue(tree)
      case _ => continue(tree)
    }
  }

  implicit class TreeExtensions(tree: Tree) {
    def getTypeSymbol: Symbol = tree match {
      case tree: ImplDef => tree.symbol
      case tree: ValOrDefDef => tree.tpt.symbol
      case tree if tree.tpe != null => tree.tpe.typeSymbol
      case _ => null
    }

    def isFunction: Boolean = {
      def isFunctionSymbol(symbol: Symbol): Boolean =
        symbol.qualifiedName.startsWith("scala.Function")

      val symbol = tree.getTypeSymbol
      symbol != null && (isFunctionSymbol(symbol) || symbol.parentSymbols.exists(isFunctionSymbol))
    }

    def isUnit: Boolean = {
      val symbol = tree.getTypeSymbol
      symbol != null && symbol.qualifiedName == "scala.Unit"
    }

    def isMutableType: Boolean = {
      val symbol = tree.getTypeSymbol
      symbol.qualifiedName.startsWith("java.") || symbol.qualifiedName.contains("mutable")
    }

    def isLazy: Boolean = tree.symbol != null && tree.symbol.isLazy

    def isVal: Boolean = tree.symbol != null && tree.symbol.kindString.contains("value")

    def isVar: Boolean = tree.symbol != null && tree.symbol.kindString.contains("variable")

    def isValOrVar: Boolean = tree.isVal || tree.isVar

    def isMethod: Boolean = tree.symbol != null && tree.symbol.kindString.contains("method")

    def isFor: Boolean = tree.attachments.all.contains(ForAttachment)

    def filterTraverse(f: PartialFunction[Tree, Boolean]): List[Tree] = new FilterTraverser(f).filter(tree)

    def collectTraverse[T](f: PartialFunction[Tree, T]): List[T] = new CollectTraverser(f).collect(tree)

    def findTraverse(f: PartialFunction[Tree, Boolean]): Option[Tree] = new FindTraverser(f).find(tree)

    def existsTraverse(f: PartialFunction[Tree, Boolean]): Boolean = new ExistsTraverser(f).exists(tree)

    def forallTraverse(f: PartialFunction[Tree, Boolean]): Boolean = new ForallTraverser(f).forall(tree)

    def foldTraverse[T](base: T)(f: (T, Tree) => T): T = new FoldTraverser(base)(f).fold(tree)

    def sumTraverse(f: PartialFunction[Tree, Int]): Int = new SumTraverser(f).sum(tree)

    def countTraverse(f: PartialFunction[Tree, Boolean]): Int = new CountTraverser(f).count(tree)

    def scopeFoldTraverse[T](base: T)(f: mutable.Stack[ListBuffer[String]] => (T, Tree) => T): T = new ScopeFoldTraverser(base)(f).fold(tree)

    def scopeSumTraverse(f: mutable.Stack[ListBuffer[String]] => PartialFunction[Tree, Int]): Int = new ScopeSumTraverser(f).sum(tree)

    def scopeCountTraverse(f: mutable.Stack[ListBuffer[String]] => PartialFunction[Tree, Boolean]): Int = new ScopeCountTraverser(f).count(tree)

    def parentTraverse[T](f: Option[T] => PartialFunction[Tree, T]): Option[T] = new ParentTraverser(f).top(tree)

    def fractionTraverse(f: PartialFunction[Tree, FractionPart]): Double = new FractionTraverser(f).fraction(tree)

    def linesTraverse(f: PartialFunction[Tree, Boolean]): Int = new LinesTraverser(f).lines(tree)

    def cyclomaticComplexity: Int = new CyclomaticComplexityTraverser().complexity(tree)
  }

  implicit class SymbolExtensions(symbol: Symbol) {
    def qualifiedName: String = symbol.enclosingPackage match {
      case _: NoSymbol => symbol.nameString
      case packageSymbol => packageSymbol.qualifiedName + "." + symbol.nameString
    }

    def pathString: String = symbol.ownerChain.init.reverse.mkString(" - ")
  }

}
