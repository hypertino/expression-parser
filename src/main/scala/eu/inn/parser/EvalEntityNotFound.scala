package eu.inn.parser

class EvalEntityNotFound(entityName: String) extends RuntimeException(s"$entityName isn't found")
