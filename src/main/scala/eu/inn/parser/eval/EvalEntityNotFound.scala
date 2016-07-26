package eu.inn.parser.eval

class EvalEntityNotFound(entityName: String) extends RuntimeException(s"$entityName isn't found")
