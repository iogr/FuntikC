package org.funtikc

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}
import org.arnoldc.ast._

class FuntikParser extends Parser {


  val ParseError = "Я ХОТЬ И ФОКУСНИК, НО ОБМАНЩИКОВ НЕ ТЕРПЛЮ"

  val DeclareInt = "У МЕНЯ МНОГО РАЗНЫХ ТАЛАНТОВ"
  val SetInitialValue = "ТАК… МЫ ИМЕЕМ: СБЕЖАВШЕГО ПОРОСЕНКА… И ДВУХ ЛУЧШИХ СЫЩИКОВ… НЕБОЛЬШОГО УМА"
  val BeginMain = "НАЧИНАЦИЮ ОПЕРАЮ! ТО ЕСТЬ ОПЕРАЦИЮ НАЧИНАЮ…"
  val PlusOperator = "ПОДАЙТЕ НА ДОМИКИ ДЛЯ БЕЗДОМНЫХ ПОРОСЯТ"
  val MinusOperator = "ДЕТИ ПЛАЧУТ, А РОДИТЕЛИ ПЛАТЯТ"
  val MultiplicationOperator = "МНЕ БЫЛО БЫ ТАК ОДИНОКО БЕЗ МОЕГО МИЛЛИОНА"
  val DivisionOperator = "ДЕТИ ПЛАЧУТ"
  val EndMain = "С НИМИ ВСЕГДА ТАК. СНАЧАЛА ТЫ НА НИХ ЕДЕШЬ, ПОТОМ ОНИ НА ТЕБЕ"
  val Print = "ФУНТИК, ПИШИ НАМ! ПИШИ НАМ ПО АДРЕСУ… (ВСХЛИПЫВАЯ) АВТОМОБИЛЬЧИК ДЯДЮШКИ МОКУСА… ФОКУСУ-МОКУСУ… ЛИЧНО В РУКИ"
  val Read = "ГДЕ ФУНТИК?? ГДЕ МОЙ ПОРОСЕНОЧЕК"
  val AssignVariable = "ДОХРЮКАЛСЯ"
  val SetValue = "ДАМА ПРИЯТНОЙ НАРУЖНОСТИ"
  val EndAssignVariable = "ПОРОСЕНОК АРЕСТОВАН! ЯЩИК — КОНФИСКОВАН"
  val False = "ВОТ ТАК Я ОБМАНУЛ ОДИННАДЦАТЬ МАЛЬЧИКОВ, ПЯТНАДЦАТЬ ДЕВОЧЕК И ОДНОГО ОЧЕНЬ ДОБРОГО СТАРИЧКА"
  val True = "КОНЕЧНО, ДА"
  val EqualTo = "ПОЗВОЛЬТЕ"
  val GreaterThan = "НЕ ПОЗВОЛЮ"
  val Or = "МЕНЬШЕ МОЖНО, БОЛЬШЕ - НИ-НИ"
  val And = "РЫБУ НАДО ЛОВИТЬ В РЕЧКЕ"
  val If = "ГДЕ НАДО ЛОВИТЬ РЫБУ"
  val Else = "АРТИСТ — ЭТО ЗВУЧИТ ГОРДО"
  val EndIf = "ЩЕКОТКИ БОИТСЯ, НО КУШАЕТ ХОРОШО"
  val While = "ВНЮХНИ"
  val EndWhile = "ВЫНЮХНИ"
  val DeclareMethod = "ФОКУСЫ — ЭТО ЛОВКОСТЬ РУК И НИКАКОГО МОШЕНСТВА"
  val MethodArguments = "ЛУЧШИЙ КОФЕ НА ДОРОГЕ: ОТХЛЕБНЕШЬ — ПРОТЯНЕШЬ НОГИ"
  val Return = "ГДЕ МОЯ СКАЗОЧКА"
  val EndMethodDeclaration = "СДАЛИ"
  val CallMethod = "МЫ — АЛЕ-ГОП, А ОНА — ВУАЛЯ"
  val NonVoidMethod = "МОЖЕТ БЫТЬ, У НИХ ЕСТЬ МАЛЕНЬКИЕ ДЕТИ, КОТОРЫЕ ЛЮБЯТ ЦИРК"
  val AssignVariableFromMethodCall = "МАДАМ, ПОДАЙТЕ НАМ РУКУ"
  val Modulo = "ОНИ НАС НЕ УЗНАЛИ! МЫ — ОХОТНИК С СОБАКОЙ"

  val EOL = zeroOrMore("\t" | "\r" | " ") ~ "\n" ~ zeroOrMore("\t" | "\r" | " " | "\n")
  val WhiteSpace = oneOrMore(" " | "\t")

  def Root: Rule1[RootNode] = rule {
    oneOrMore(AbstractMethod) ~ EOI ~~> RootNode
  }

  def AbstractMethod: Rule1[AbstractMethodNode] = rule {
    (MainMethod | Method) ~ optional(EOL)
  }

  def MainMethod: Rule1[AbstractMethodNode] = rule {
    BeginMain ~ EOL ~ zeroOrMore(Statement) ~ EndMain ~~> MainMethodNode
  }

  def Method: Rule1[AbstractMethodNode] = rule {
    DeclareMethod ~ WhiteSpace ~ VariableName ~> (s => s) ~ EOL ~
      zeroOrMore((MethodArguments ~ WhiteSpace ~ Variable ~ EOL)) ~
      (NonVoidMethod | "") ~> ((m: String) => m == NonVoidMethod) ~ optional(EOL) ~
      zeroOrMore(Statement) ~ EndMethodDeclaration ~~> MethodNode
  }

  def Statement: Rule1[StatementNode] = rule {
    DeclareIntStatement | PrintStatement |
      AssignVariableStatement | ConditionStatement |
      WhileStatement | CallMethodStatement | ReturnStatement | CallReadMethodStatement
  }

  def CallMethodStatement: Rule1[StatementNode] = rule {
    (AssignVariableFromMethodCall ~ WhiteSpace ~ VariableName ~> (v => v) ~ EOL | "" ~> (v => v)) ~
      CallMethod ~ WhiteSpace ~ VariableName ~> (v => v) ~
      zeroOrMore(WhiteSpace ~ Operand) ~ EOL ~~> CallMethodNode
  }

  def CallReadMethodStatement: Rule1[StatementNode] = rule {
    (AssignVariableFromMethodCall ~ WhiteSpace ~ VariableName ~> (v => v) ~ EOL | "" ~> (v => v)) ~
      CallMethod ~ EOL ~ Read ~ EOL ~~> CallReadMethodNode
  }

  def ConditionStatement: Rule1[ConditionNode] = rule {
    If ~ WhiteSpace ~ Operand ~ EOL ~ zeroOrMore(Statement) ~
      (Else ~ EOL ~ zeroOrMore(Statement) ~~> ConditionNode
        | zeroOrMore(Statement) ~~> ConditionNode) ~ EndIf ~ EOL

  }

  def WhileStatement: Rule1[WhileNode] = rule {
    While ~ WhiteSpace ~ Operand ~ EOL ~ zeroOrMore(Statement) ~ EndWhile ~ EOL ~~> WhileNode
  }

  def PrintStatement: Rule1[PrintNode] = rule {
    Print ~ WhiteSpace ~ (Operand ~~> PrintNode | "\"" ~ String ~ "\"" ~~> PrintNode) ~ EOL
  }

  def DeclareIntStatement: Rule1[DeclareIntNode] = rule {
    DeclareInt ~ WhiteSpace ~ VariableName ~> (s => s) ~ EOL ~ SetInitialValue ~ WhiteSpace ~ Operand ~~> DeclareIntNode ~ EOL
  }

  def AssignVariableStatement: Rule1[AssignVariableNode] = rule {
    AssignVariable ~ WhiteSpace ~ VariableName ~> (s => s) ~ EOL ~ Expression ~ EndAssignVariable ~ EOL ~~> AssignVariableNode
  }

  def ReturnStatement: Rule1[StatementNode] = rule {
    Return ~ ((WhiteSpace ~ Operand ~~> (o => ReturnNode(Some(o)))) | "" ~> (s => ReturnNode(None))) ~ EOL
  }

  def Operand: Rule1[OperandNode] = rule {
    Number | Variable | Boolean
  }

  def Expression: Rule1[AstNode] = rule {
    SetValueExpression ~
      (zeroOrMore(ArithmeticOperation | LogicalOperation))
  }

  def LogicalOperation: ReductionRule1[AstNode, AstNode] = rule {
    Or ~ WhiteSpace ~ Operand ~ EOL ~~> OrNode |
      And ~ WhiteSpace ~ Operand ~ EOL ~~> AndNode |
      EqualTo ~ WhiteSpace ~ Operand ~ EOL ~~> EqualToNode |
      GreaterThan ~ WhiteSpace ~ Operand ~ EOL ~~> GreaterThanNode

  }

  def RelationalExpression: ReductionRule1[AstNode, AstNode] = {
    EqualToExpression ~~> EqualToNode |
      GreaterThanExpression ~~> GreaterThanNode
  }


  def EqualToExpression: Rule1[OperandNode] = {
    EqualTo ~ WhiteSpace ~ Operand ~ EOL
  }

  def GreaterThanExpression: Rule1[OperandNode] = {
    GreaterThan ~ WhiteSpace ~ Operand ~ EOL
  }

  def ArithmeticOperation: ReductionRule1[AstNode, AstNode] = rule {
    PlusExpression ~~> PlusExpressionNode |
      MinusExpression ~~> MinusExpressionNode |
      MultiplicationExpression ~~> MultiplicationExpressionNode |
      DivisionExpression ~~> DivisionExpressionNode |
      ModuloExpression ~~> ModuloExpressionNode
  }

  def SetValueExpression: Rule1[OperandNode] = rule {
    SetValue ~ WhiteSpace ~ Operand ~ EOL
  }


  def PlusExpression: Rule1[AstNode] = rule {
    PlusOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def MinusExpression: Rule1[AstNode] = rule {
    MinusOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def MultiplicationExpression: Rule1[AstNode] = rule {
    MultiplicationOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def DivisionExpression: Rule1[AstNode] = rule {
    DivisionOperator ~ WhiteSpace ~ Operand ~ EOL
  }

  def ModuloExpression: Rule1[AstNode] = rule {
    Modulo ~ WhiteSpace ~ Operand ~ EOL
  }

  def Variable: Rule1[VariableNode] = rule {
    VariableName ~> VariableNode
  }

  def VariableName: Rule0 = rule {
    rule("A" - "Z" | "a" - "z") ~ zeroOrMore("A" - "Z" | "a" - "z" | "0" - "9")
  }

  def Number: Rule1[NumberNode] = rule {
    oneOrMore("0" - "9") ~> ((matched: String) => NumberNode(matched.toInt)) |
      "-" ~ oneOrMore("0" - "9") ~> ((matched: String) => NumberNode(-matched.toInt))
  }

  def Boolean: Rule1[NumberNode] = rule {
    "@" ~ True ~> (_ => NumberNode(1)) |
      "@" ~ False ~> (_ => NumberNode(0))
  }

  def String: Rule1[StringNode] = rule {
    zeroOrMore(rule {
      !anyOf("\"\\") ~ ANY
    }) ~> StringNode
  }

  def parse(expression: String): RootNode = {
    val parsingResult = ReportingParseRunner(Root).run(expression)
    parsingResult.result match {
      case Some(root) => root
      case None => throw new ParsingException(ParseError + ":\n" +
        ErrorUtils.printParseErrors(parsingResult))
    }
  }

}