package org.jetbrains.plugins.scala
package lang
package psi
package impl
package statements

import com.intellij.lang.ASTNode
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.project.DumbService
import com.intellij.psi.PsiReferenceList.Role
import com.intellij.psi._
import com.intellij.psi.impl.source.HierarchicalMethodSignatureImpl
import com.intellij.psi.scope.PsiScopeProcessor
import com.intellij.psi.tree.TokenSet
import com.intellij.psi.util.MethodSignatureBackedByPsiMethod
import com.intellij.util.PlatformIcons
import com.intellij.util.containers.ContainerUtil
import javax.swing.Icon
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.extensions.{PsiClassExt, PsiModifierListOwnerExt, PsiTypeExt, TraversableExt}
import org.jetbrains.plugins.scala.icons.Icons
import org.jetbrains.plugins.scala.lang.lexer._
import org.jetbrains.plugins.scala.lang.parser.ScalaElementType
import org.jetbrains.plugins.scala.lang.psi.api.ScalaFile
import org.jetbrains.plugins.scala.lang.psi.api.base.types.ScTypeElement
import org.jetbrains.plugins.scala.lang.psi.api.expr.ScBlock
import org.jetbrains.plugins.scala.lang.psi.api.statements._
import org.jetbrains.plugins.scala.lang.psi.api.statements.params._
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.ScTypeParametersOwner
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.templates.ScExtendsBlock
import org.jetbrains.plugins.scala.lang.psi.api.toplevel.typedef.{ScClass, ScMember, ScTrait, ScTypeDefinition}
import org.jetbrains.plugins.scala.lang.psi.fake.FakePsiReferenceList
import org.jetbrains.plugins.scala.lang.psi.impl.ScalaPsiElementFactory.createIdentifier
import org.jetbrains.plugins.scala.lang.psi.impl.statements.ScFunctionImpl.isJavaVarargs
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.synthetic.{JavaIdentifier, SyntheticClasses}
import org.jetbrains.plugins.scala.lang.psi.impl.toplevel.typedef.TypeDefinitionMembers
import org.jetbrains.plugins.scala.lang.psi.light.ScFunctionWrapper
import org.jetbrains.plugins.scala.lang.psi.stubs.ScFunctionStub
import org.jetbrains.plugins.scala.lang.psi.stubs.elements.ScFunctionElementType
import org.jetbrains.plugins.scala.lang.psi.types.api.{FunctionType, ParameterizedType, Unit}
import org.jetbrains.plugins.scala.lang.psi.types.nonvalue.ScMethodType
import org.jetbrains.plugins.scala.lang.psi.types.recursiveUpdate.ScSubstitutor
import org.jetbrains.plugins.scala.lang.psi.types.result.{Failure, TypeResult}
import org.jetbrains.plugins.scala.lang.psi.types.{PhysicalMethodSignature, ScType, TermSignature}
import org.jetbrains.plugins.scala.macroAnnotations.{Cached, CachedInUserData, ModCount}

import scala.annotation.tailrec
import scala.collection.Seq
import scala.collection.mutable.ArrayBuffer

/**
  * @author ilyas
  */
abstract class ScFunctionImpl[F <: ScFunction](stub: ScFunctionStub[F],
                                               nodeType: ScFunctionElementType[F],
                                               node: ASTNode)
  extends ScalaStubBasedElementImpl(stub, nodeType, node)
    with ScMember
    with ScFunction
    with ScTypeParametersOwner {

  override def isStable = false

  def nameId: PsiElement = {
    val n = getNode.findChildByType(ScalaTokenTypes.tIDENTIFIER) match {
      case null => getNode.findChildByType(ScalaTokenTypes.kTHIS)
      case notNull => notNull
    }
    if (n == null) {
      val stub = getGreenStub
      if (stub == null) {
        val message = ScalaBundle.message("both.stub.and.name.identifier.node.are.null", getClass.getSimpleName, getText)
        throw new NullPointerException(message)
      }
      return createIdentifier(getGreenStub.getName).getPsi
    }
    n.getPsi
  }

  @Cached(ModCount.anyScalaPsiModificationCount, this)
  def paramClauses: ScParameters = getStubOrPsiChild(ScalaElementType.PARAM_CLAUSES)

  override def processDeclarations(processor: PsiScopeProcessor, state: ResolveState,
                                   lastParent: PsiElement, place: PsiElement): Boolean = {
    if (lastParent == null) return true

    // process function's type parameters
    if (!super[ScTypeParametersOwner].processDeclarations(processor, state, lastParent, place)) return false

    processParameters(processor, state, lastParent)
  }

  private def processParameters(processor: PsiScopeProcessor,
                                state: ResolveState,
                                lastParent: PsiElement): Boolean = {

    if (lastParent != null && shouldProcessParameters(lastParent)) {
      for {
        clause <- effectiveParameterClauses
        param <- clause.effectiveParameters
      } {
        ProgressManager.checkCanceled()
        if (!processor.execute(param, state)) return false
      }
    }
    true
  }

  // to resolve parameters in return type, type parameter context bounds and body;
  // references in default parameters are processed in ScParametersImpl
  protected def shouldProcessParameters(lastParent: PsiElement): Boolean = {
    def isFromTypeParams = lastParent.isInstanceOf[ScTypeParamClause]

    //don't compare returnTypeElement with lastParent, they may be different instances due to caches/stubs
    def isReturnTypeElement = lastParent.isInstanceOf[ScTypeElement] && lastParent.getContext == this

    !lastParent.isPhysical || isFromTypeParams || isReturnTypeElement
  }

  @Cached(ModCount.anyScalaPsiModificationCount, this)
  def returnTypeElement: Option[ScTypeElement] = byPsiOrStub(findChild(classOf[ScTypeElement]))(_.typeElement)

  // TODO unify with ScValue and ScVariable
  protected override final def baseIcon: Icon = {
    var parent = getParent
    while (parent != null) {
      parent match {
        case _: ScExtendsBlock =>
          return if (isAbstractMember) PlatformIcons.ABSTRACT_METHOD_ICON else PlatformIcons.METHOD_ICON
        case (_: ScBlock | _: ScalaFile) => return Icons.FUNCTION
        case _ => parent = parent.getParent
      }
    }
    null
  }

  def getReturnType: PsiType = {
    if (DumbService.getInstance(getProject).isDumb || !SyntheticClasses.get(getProject).isClassesRegistered) {
      return null //no resolve during dumb mode or while synthetic classes is not registered
    }
    if(isConstructor) {
      null
    } else {
      getReturnTypeImpl
    }
  }

  @CachedInUserData(this, ModCount.getBlockModificationCount)
  private def getReturnTypeImpl: PsiType = {
    val resultType = `type`().getOrAny match {
      case FunctionType(rt, _) => rt
      case tp => tp
    }
    resultType.toPsiType
  }

  def definedReturnType: TypeResult = {
    returnTypeElement match {
      case Some(ret) => ret.`type`()
      case _ if !hasAssign => Right(Unit)
      case _ =>
        superMethod match {
          case Some(f: ScFunction) => f.definedReturnType
          case Some(m: PsiMethod) =>
            Right(m.getReturnType.toScType())
          case _ => Failure(ScalaBundle.message("no.defined.return.type"))
        }
    }
  }

  def hasUnitResultType: Boolean = {
    @tailrec
    def hasUnitRT(t: ScType): Boolean = t match {
      case _ if t.isUnit => true
      case ScMethodType(result, _, _) => hasUnitRT(result)
      case _ => false
    }
    this.returnType.exists(hasUnitRT)
  }

  def hasParameterClause: Boolean = ScFunctionImpl.hasParameterClauseImpl(this)

  def parameterListCount: Int = paramClauses.clauses.length

  @CachedInUserData(this, ModCount.getBlockModificationCount)
  def effectiveParameterClauses: Seq[ScParameterClause] = {
    val maybeOwner = if (isConstructor) {
      containingClass match {
        case owner: ScTypeParametersOwner => Some(owner)
        case _ => None
      }
    } else Some(this)

    paramClauses.clauses ++ maybeOwner.flatMap {
      ScalaPsiUtil.syntheticParamClause(_, paramClauses, isClassParameter = false)()
    }
  }

  /**
    * @return Empty array, if containing class is null.
    */
  @Cached(ModCount.getBlockModificationCount, this)
  def getFunctionWrappers(isStatic: Boolean, isAbstract: Boolean, cClass: Option[PsiClass] = None): Seq[ScFunctionWrapper] = {
    val buffer = new ArrayBuffer[ScFunctionWrapper]
    if (cClass.isDefined || containingClass != null) {
      for {
        clause <- clauses
        first  <- clause.clauses.headOption
        if first.hasRepeatedParam && isJavaVarargs(this)
      } buffer += new ScFunctionWrapper(this, isStatic, isAbstract, cClass, isJavaVarargs = true)

      buffer += new ScFunctionWrapper(this, isStatic, isAbstract, cClass)
    }
    buffer
  }

  // TODO Should be unified, see ScModifierListOwner
  override def hasModifierProperty(name: String): Boolean = {
    if (name == "abstract") {
      this match {
        case _: ScFunctionDeclaration =>
          containingClass match {
            case _: ScTrait => return true
            case c: ScClass if c.hasAbstractModifier => return true
            case _ =>
          }
        case _ =>
      }
    }
    super.hasModifierProperty(name)
  }

  def hasAssign: Boolean = getNode.getChildren(TokenSet.create(ScalaTokenTypes.tASSIGN)).nonEmpty

  override def getNameIdentifier: PsiIdentifier = new JavaIdentifier(nameId)

  def findDeepestSuperMethod: PsiMethod = {
    val s = superMethods
    if (s.isEmpty) null
    else s.last
  }

  def getReturnTypeElement: Null = null

  def findSuperMethods(parentClass: PsiClass): Array[PsiMethod] = PsiMethod.EMPTY_ARRAY

  def findSuperMethods(checkAccess: Boolean): Array[PsiMethod] = PsiMethod.EMPTY_ARRAY

  def findSuperMethods: Array[PsiMethod] = superMethods.toArray // TODO which other xxxSuperMethods can/should be implemented?

  def findDeepestSuperMethods: Array[PsiMethod] = PsiMethod.EMPTY_ARRAY

  def getReturnTypeNoResolve: PsiType = PsiType.VOID

  def findSuperMethodSignaturesIncludingStatic(checkAccess: Boolean): java.util.List[MethodSignatureBackedByPsiMethod] =
    ContainerUtil.emptyList()

  def getSignature(substitutor: PsiSubstitutor): MethodSignatureBackedByPsiMethod = MethodSignatureBackedByPsiMethod.create(this, substitutor)

  //todo implement me!
  def isVarArgs = false

  def isConstructor: Boolean = name == "this"

  def getBody: PsiCodeBlock = null

  def getThrowsList: FakePsiReferenceList = new FakePsiReferenceList(getManager, getLanguage, Role.THROWS_LIST) {
    override def getReferenceElements: Array[PsiJavaCodeReferenceElement] = {
      getReferencedTypes.map {
        tp => PsiElementFactory.getInstance(getProject).createReferenceElementByType(tp)
      }
    }

    override def getReferencedTypes: Array[PsiClassType] = {
      annotations("scala.throws").headOption match {
        case Some(annotation) =>
          annotation.constructorInvocation.args.map(_.exprs).getOrElse(Seq.empty).flatMap {
            _.`type`() match {
              case Right(ParameterizedType(des, Seq(arg))) => des.extractClass match {
                case Some(clazz) if clazz.qualifiedName == "java.lang.Class" =>
                  arg.toPsiType match {
                    case c: PsiClassType => Seq(c)
                    case _ => Seq.empty
                  }
                case _ => Seq.empty
              }
              case _ => Seq.empty
            }
          }.toArray
        case _ => PsiClassType.EMPTY_ARRAY
      }
    }
  }

  def `type`(): TypeResult = {
    this.returnType match {
      case Right(tp) =>
        var res: TypeResult = Right(tp)
        val paramClauses = effectiveParameterClauses
        var i = paramClauses.length - 1
        while (i >= 0) {
          res match {
            case Right(t) =>
              val parameters = paramClauses.apply(i).effectiveParameters
              val paramTypes = parameters.map(_.`type`().getOrNothing)
              res = Right(FunctionType(t, paramTypes))
            case _ =>
          }
          i = i - 1
        }
        res
      case x => x
    }
  }

  override protected def isSimilarMemberForNavigation(m: ScMember, strictCheck: Boolean): Boolean = m match {
    case f: ScFunction => f.name == name && {
      if (strictCheck) new PhysicalMethodSignature(this, ScSubstitutor.empty).
        paramTypesEquiv(new PhysicalMethodSignature(f, ScSubstitutor.empty))
      else true
    }
    case _ => false
  }

  def getHierarchicalMethodSignature: HierarchicalMethodSignature = {
    new HierarchicalMethodSignatureImpl(getSignature(PsiSubstitutor.EMPTY))
  }

  override def getOriginalElement: PsiElement = {
    val ccontainingClass = containingClass
    if (ccontainingClass == null) return this
    val originalClass: PsiClass = ccontainingClass.getOriginalElement.asInstanceOf[PsiClass]
    if (ccontainingClass eq originalClass) return this
    if (!originalClass.isInstanceOf[ScTypeDefinition]) return this
    val c = originalClass.asInstanceOf[ScTypeDefinition]
    val membersIterator = c.members.iterator
    val buf: ArrayBuffer[ScMember] = new ArrayBuffer[ScMember]
    while (membersIterator.hasNext) {
      val member = membersIterator.next()
      if (isSimilarMemberForNavigation(member, strictCheck = false)) buf += member
    }
    if (buf.isEmpty) this
    else if (buf.length == 1) buf(0)
    else {
      val filter = buf.filter(isSimilarMemberForNavigation(_, strictCheck = true))
      if (filter.isEmpty) buf(0)
      else filter(0)
    }
  }

  def superMethods: Seq[PsiMethod] = superSignatures.map(_.namedElement).filterBy[PsiMethod]

  def superMethod: Option[PsiMethod] = superMethodAndSubstitutor.map(_._1)

  def superMethodAndSubstitutor: Option[(PsiMethod, ScSubstitutor)] = {
    val option = TypeDefinitionMembers.getSignatures(containingClass).forName(name).findNode(this)
    option
      .flatMap(_.primarySuper)
      .map(_.info)
      .collect {
        case p: PhysicalMethodSignature => (p.method, p.substitutor)
      }
  }


  def superSignatures: Seq[TermSignature] = {
    TypeDefinitionMembers.getSignatures(containingClass).forName(name).findNode(this) match {
      case Some(x) => x.supers.map {_.info}
      case None => Seq.empty
    }
  }

  def superSignaturesIncludingSelfType: Seq[TermSignature] = {
    val clazz = containingClass
    if (clazz == null) return Seq.empty

    if (clazz.selfType.isDefined) {
      val signs = TypeDefinitionMembers.getSelfTypeSignatures(clazz).forName(name)
      signs.findNode(this) match {
        case Some(x) if x.info.namedElement == this => x.supers.map(_.info)
        case Some(x) => x.supers.filter(_.info.namedElement != this).map(_.info) :+ x.info
        case None => signs.get(new PhysicalMethodSignature(this, ScSubstitutor.empty)) match {
          case Some(x) if x.info.namedElement == this => x.supers.map(_.info)
          case Some(x) => x.supers.filter(_.info.namedElement != this).map(_.info) :+ x.info
          case None => Seq.empty
        }
      }
    } else superSignatures
  }

}

object ScFunctionImpl {

  @tailrec
  private def hasParameterClauseImpl(function: ScFunction): Boolean = {
    if (function.effectiveParameterClauses.nonEmpty) return true

    function.superMethod match {
      case Some(fun: ScFunction) => hasParameterClauseImpl(fun)
      case Some(_: PsiMethod) => true
      case None => false
    }
  }

  @tailrec
  private def isJavaVarargs(fun: ScFunction): Boolean = {
    if (fun.hasAnnotation("scala.annotation.varargs")) true
    else {
      fun.superMethod match {
        case Some(f: ScFunction) => isJavaVarargs(f)
        case Some(m: PsiMethod) => m.isVarArgs
        case _ => false
      }
    }
  }

}