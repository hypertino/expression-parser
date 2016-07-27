package eu.inn.parser.eval

class EvaluatorEngineWithContext(val context: Context) extends EvaluatorEngine {
  override def identifier = context.identifier orElse super.identifier
  override def function = context.function orElse super.function
}
