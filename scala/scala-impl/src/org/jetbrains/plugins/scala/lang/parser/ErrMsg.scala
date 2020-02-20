package org.jetbrains.plugins.scala
package lang
package parser

import org.jetbrains.annotations.PropertyKey

/**
* @author ilyas
*/

object ErrMsg{
  //noinspection DynamicPropertyKey
  def apply(@PropertyKey(resourceBundle = "messages.ScalaBundle") msg: String): String =
    ScalaBundle.message(msg)
}