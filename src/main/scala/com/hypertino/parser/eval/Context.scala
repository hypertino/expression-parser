package com.hypertino.parser.eval

import com.hypertino.binders.value.Value
import com.hypertino.parser.ast.Identifier

trait Context {
  def identifier: PartialFunction[Identifier, Value]
  def function: PartialFunction[(Identifier, Seq[Value]), Value]
  def unaryOperation: PartialFunction[(Identifier, Value), Value]
  def binaryOperation: PartialFunction[(Value, Identifier, Value), Value]
  def customOperators: Seq[String]
}
