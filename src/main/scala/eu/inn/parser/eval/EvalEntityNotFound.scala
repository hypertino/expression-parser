package eu.inn.parser.eval

class EvalEntityNotFound(entityName: String) extends RuntimeException(s"$entityName isn't found")
class EvalIdentifierNotFound(identifier: String) extends EvalEntityNotFound(identifier)
class EvalFunctionNotFound(function: String) extends EvalEntityNotFound(function)
class EvalBinaryOperationNotFound(operation: String) extends EvalEntityNotFound(operation)
class EvalUnaryOperationNotFound(operation: String) extends EvalEntityNotFound(operation)