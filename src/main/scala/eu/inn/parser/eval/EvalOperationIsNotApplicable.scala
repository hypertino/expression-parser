package eu.inn.parser.eval

class EvalOperationIsNotApplicable(operation: String) extends RuntimeException(s"$operation is not applicable")
