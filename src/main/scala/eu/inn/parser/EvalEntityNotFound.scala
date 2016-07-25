package eu.inn.parser

class EvalEntityNotFound(entityName: String, entityType: String) extends RuntimeException(s"$entityType $entityName isn't found")
class BinaryOperatorNotFound(entityName: String) extends EvalEntityNotFound(entityName, "Binary operator")
class UnaryOperatorNotFound(entityName: String) extends EvalEntityNotFound(entityName, "Unary operator")
class IdentifierNotFound(entityName: String) extends EvalEntityNotFound(entityName, "Identifier")
class FunctionNotFound(entityName: String) extends EvalEntityNotFound(entityName, "Function")