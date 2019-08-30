package org.jetbrains.plugins.scala
package lang
package parser

import com.intellij.openapi.project.Project
import com.intellij.psi.FileViewProvider

final class Scala3ParserDefinition extends ScalaParserDefinitionBase(
  new psi.stubs.elements.ScStubFileElementType("scala3.file", Scala3Language.INSTANCE)
) {

  override def createFile(viewProvider: FileViewProvider) =
    new psi.impl.ScalaFileImpl(viewProvider)

  override def createParser(project: Project) = new ScalaParser(isScala3 = true)
}
