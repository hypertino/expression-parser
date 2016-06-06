# expession-parser

Парсер разбирает и проверяет на истинность выражения.

## Поддерживаемые операции:
стандартные логические операции: "!, =, !=, and, or" 
"someObject.someField" - вычисление значения выражения
"has" - проверка наличия элемента в коллекции
"and" - конъюнкция результатов друх выражений
"or" - дизъюнкция результатов друх выражений

## Примеры:
```scala
user.isDefined
!user.isDefined
user.isDefined = true 
user.isDefined != true
user.roles has "qa" 
(user.isDefined = true) and (user.roles has "qa") 
(user.isDefined) and (user.roles has "qa")) 
(!user.isDefined) or (user.roles has "admin")
```

## Ограничения:
1. На данный момент подвыражения, соединенные с помощью `and` или `or` должны быть заключены в скобки
2. Reflection при вычислении выражений не используется, поэтому для корректного вычисления выражений необходимо инициализировать EvaluationEngine 
объектом типа Map[String, Any], содержащим значения всех полей. Например:
```scala
val evalEngine = new MapBasedEvaluationEngine(Map(
  "user" → Map(
    "isDefined" → true,
    "roles" → Seq("dev", "qa", "ops")
  )
))
```