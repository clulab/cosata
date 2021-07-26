package inferenceengine.iml.parser

import inferenceengine.iml.model.{AddExplanationText, AttachRowToInstance, Break, CellExprElem, CellPattern, CellRef, CellRefExpr, CellText, CellVariable, Change, ChangeElem, CompInfPatRef, CompInfPatReq, CompInstEquivalency, CompRowEquivalency, Condition, ConditionChange, ConditionElem, ConditionExpr, ConditionLR, ConditionLRDouble, ConditionLRStr, ConditionOperator, DetachRowFromInstance, ExecTemporalBlockList, ExecTemporalElem, ExecuteAutoPatterns, ExecuteInfPattern, Exit, ExportInferencePatternDebugHTML, ExportInferencePatternsHTML, ExportInferencePatternsJSON, ExportStateSpaceHTML, ExportTableStoreHTML, Expr, Function, GenerateRow, GetStateWithName, Identifier, IfStatement, IfStatementCondition, ImportFile, ImportFileOrFolder, ImportFolder, IncrementState, InfConstraint, InfConstraintMustHave, InfConstraintMustHaveOrOmit, InfConstraintShouldHave, InfInstanceReq, InfPatDescription, InfPatExecutionMode, InfPattern, InstRemap, InstanceDefinition, InstanceProperty, InstancePropertyAssignment, InstanceRequirement, InstantiateCommonProperties, InstantiateWithProperties, MeetsRequirementsInfPattern, Number, Operator, PatternMatchDescription, PopulateInfPatMatches, Print, PrintInstances, PrintState, PrintVariables, Program, RemoveRowUUID, RemoveRowsRowRef, RowDef, RowRef, RowRefExpr, SetEnvironmentVariable, SetStateName, StateAfterName, StateBeforeName, StateFirstWithName, StateLastWithName, StateRef, StateWithName, Statement, StatementInf, Str, TableRef, TemporalBlock, UUIDRef, VariableAssignment, VariableConstantText, VariableDefinition, VerifyAnswer, WhileLoop}
import inferenceengine.struct.{InferencePattern, ProcessStageComplete, ProcessStageTransition}

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers


/**
  * Based on the helpful interpreter tutorial by Travis Dazell ( https://dzone.com/articles/create-a-programming-language-with-scala-parser-co )
  * Created by peter on 7/9/18.
  */
class IMLParser extends StandardTokenParsers {
  lexical.reserved += ("var", "println", "if", "then", "else", "endif", "function", "return", "endfunction", "main", "inferencepattern", "endinferencepattern",
                        "row", "TABLE", "UUID", "const", "generateRow", "removeRow", "removeRows", "description", "printInstances", "instance", "KINDOF", "CHANGE", "from", "to", "direction", "threshold",
                        "require", "shouldhave", "musthave", "musthaveoromit", "executePattern", "meetsRequirements", "exportInfPatHTML", "exportInfPatJSON", "exportTableStoreHTML", "exportInfPatDebugHTML", "populateInfPatMatches", "infpat", "rowequiv", "instmap",
                        "import", "importFolder", "executionmode", "executeAutoPatterns", "temporalblock", "endtemporalblock", "printVariables", "temporalblocks",
                        "process", "stage", "endprocess", "processStageTransition", "processStageComplete", "addExplanationText", "printState",
                        "attachRowToInstance", "detachRowFromInstance", "patterndescription",
                        "while", "endwhile", "break", "exit", "verifyAnswer", "instantiate", "props",
                        "exportStateSpaceHTML", "incrementState", "setStateName", "getStateWithName", "getStateFirstWithName", "getStateLastWithName", "getStateBeforeName", "getStateAfterName",
                        "setEnvironmentVariable")
  lexical.delimiters += ("*", "/", "%", "+", "-", "(", ")", "=", "<", ">", "==", "!=", "<=", ">=", ",", ":", "[", "]", "\"", "'", ".", "&&", "||", "!", "->", "~")


  /*
   * Program
   */
  //def program:Parser[Program] = rep(function) ~ rep(infpattern) ~ ("main" ~ ":") ~ codeblock ^^ {
  def program:Parser[Program] = rep(importfileorfolder) ~ rep(function) ~ rep(infpattern | processpattern) ~ codeblock ^^ {
    case imp ~ f ~ inf ~ c => new Program(imp, f, inf, c)
  }

  /*
   * Functions
   */
  def function:Parser[Function] = ("function" ~> ident) ~ ("(" ~> arguments) ~ (")" ~> codeblock) ~ opt(returnStatement) <~ "endfunction" ^^ {
    // Functions without return statements implicitly return 0
    case a ~ b ~ c ~ None => new Function(a, b, c, Number(0))
    // Functions with return statements
    case a ~ b ~ c ~ d => new Function(a, b, c, d.get)
  }

  def returnStatement:Parser[Expr] = "return" ~> expr ^^ {
    e => e
  }

  def arguments:Parser[Map[String, Int]] = repsep(ident, ",") ^^ {
    argumentList => {
      (for (a <- argumentList) yield (a -> 0)) toMap
    }
  }

  /*
   * Imports
   */
  def importfileorfolder:Parser[ImportFileOrFolder] = (importfile | importfolder) ^^ {
    a => a
  }

  def importfile:Parser[ImportFile] = ("import" ~> stringLit) ^^ {
    case filename => new ImportFile(filename)
  }

  def importfolder:Parser[ImportFolder] = ("importFolder" ~> stringLit) ^^ {
    case path => new ImportFolder(path)
  }

  /*
   * Executing inference patterns
   */
  def executePattern:Parser[ExecuteInfPattern] = "executePattern" ~ ("(" ~> ident) ~ ("," ~> numericLit) ~ opt("," ~> "[" ~> instMap <~ "]") ~ opt("," ~> execTemporalBlockList) <~ ")" ^^ {
    // Index (offset)-based reference
    case "executePattern" ~ patname ~ patmatchidx ~ None ~ None => new ExecuteInfPattern(patname, Some(patmatchidx.toInt), None, None, new InstRemap(List()), None)
    case "executePattern" ~ patname ~ patmatchidx ~ instremap ~ None => new ExecuteInfPattern(patname, Some(patmatchidx.toInt), None, None, instremap.get, None)
    case "executePattern" ~ patname ~ patmatchidx ~ None ~ temporalExec => new ExecuteInfPattern(patname, Some(patmatchidx.toInt), None, None, new InstRemap(List()), temporalExec)
    case "executePattern" ~ patname ~ patmatchidx ~ instremap ~ temporalExec => new ExecuteInfPattern(patname, Some(patmatchidx.toInt), None, None, instremap.get, temporalExec)
  } | "executePattern" ~ ("(" ~> ident) ~ ("," ~> stringLit) ~ opt("," ~> "[" ~> instMap <~ "]") ~ opt("," ~> execTemporalBlockList) <~ ")" ^^ {
    // Hashcode-based reference
    case "executePattern" ~ patname ~ pathashcode ~ None ~ None => new ExecuteInfPattern(patname, None, Some(pathashcode), None, new InstRemap(List()), None)
    case "executePattern" ~ patname ~ pathashcode ~ instremap ~ None => new ExecuteInfPattern(patname, None, Some(pathashcode), None, instremap.get, None)
    case "executePattern" ~ patname ~ pathashcode ~ None ~ temporalExec => new ExecuteInfPattern(patname, None, Some(pathashcode), None, new InstRemap(List()), temporalExec)
    case "executePattern" ~ patname ~ pathashcode ~ instremap ~ temporalExec => new ExecuteInfPattern(patname, None, Some(pathashcode), None, instremap.get, temporalExec)
  } | "executePattern" ~ ("(" ~> ident) ~ ("," ~> "[" ~> conditionalLRPatternExecList <~ "]") ~ opt("," ~> "[" ~> instMap <~ "]") ~ opt("," ~> execTemporalBlockList) <~ ")" ^^ {
    // Variable-list matching reference
    case "executePattern" ~ patname ~ patCondList ~ None ~ None => new ExecuteInfPattern(patname, None, None, Some(patCondList), new InstRemap(List()), None)
    case "executePattern" ~ patname ~ patCondList ~ instremap ~ None => new ExecuteInfPattern(patname, None, None, Some(patCondList), instremap.get, None)
    case "executePattern" ~ patname ~ patCondList ~ None ~ temporalExec => new ExecuteInfPattern(patname, None, None, Some(patCondList), new InstRemap(List()), temporalExec)
    case "executePattern" ~ patname ~ patCondList ~ instremap ~ temporalExec => new ExecuteInfPattern(patname, None, None, Some(patCondList), instremap.get, temporalExec)
  }

  // A list of one or more conditionalLRPatternExecs.
  // e.g. <materialName> == obj1."materialName", <SOM> == obj1."state of matter", <temperature> >= obj1."temperature"
  def conditionalLRPatternExecList:Parser[List[ConditionExpr]] = conditionExpr ~ opt(rep("," ~> conditionExpr)) ^^ {
    case first ~ None => List(first)
    case first ~ second => {
      List(first) ++ second.get
    }
  }

  /*
  def conditionalLRPatternExecList:Parser[List[Condition]] = conditionalLRPatternExec ~ opt(rep("," ~> conditionalLRPatternExec)) ^^ {
    case first ~ None => List(first)
    case first ~ second => {
      List(first) ++ second.get
    }
  }
  */
  //conditionExpr
  //conditionalLRPatternExec

  def meetsRequirements:Parser[MeetsRequirementsInfPattern] = "meetsRequirements" ~ ("(" ~> ident) ~ ("," ~> numericLit) ~ opt("," ~> "[" ~> instMap <~ "]") ~ opt("," ~> execTemporalBlockList) <~ ")" ^^ {
    // Index (offset)-based reference
    case "meetsRequirements" ~ patname ~ patmatchidx ~ None ~ None => new MeetsRequirementsInfPattern(patname, Some(patmatchidx.toInt), None, None, new InstRemap(List()), None)
    case "meetsRequirements" ~ patname ~ patmatchidx ~ instremap ~ None => new MeetsRequirementsInfPattern(patname, Some(patmatchidx.toInt), None, None, instremap.get, None)
    case "meetsRequirements" ~ patname ~ patmatchidx ~ None ~ temporalExec => new MeetsRequirementsInfPattern(patname, Some(patmatchidx.toInt), None, None, new InstRemap(List()), temporalExec)
    case "meetsRequirements" ~ patname ~ patmatchidx ~ instremap ~ temporalExec => new MeetsRequirementsInfPattern(patname, Some(patmatchidx.toInt), None, None, instremap.get, temporalExec)
  } | "meetsRequirements" ~ ("(" ~> ident) ~ ("," ~> stringLit) ~ opt("," ~> "[" ~> instMap <~ "]") ~ opt("," ~> execTemporalBlockList) <~ ")" ^^ {
    // Hashcode-based reference
    case "meetsRequirements" ~ patname ~ pathashcode ~ None ~ None => new MeetsRequirementsInfPattern(patname, None, Some(pathashcode), None, new InstRemap(List()), None)
    case "meetsRequirements" ~ patname ~ pathashcode ~ instremap ~ None => new MeetsRequirementsInfPattern(patname, None, Some(pathashcode), None, instremap.get, None)
    case "meetsRequirements" ~ patname ~ pathashcode ~ None ~ temporalExec => new MeetsRequirementsInfPattern(patname, None, Some(pathashcode), None, new InstRemap(List()), temporalExec)
    case "meetsRequirements" ~ patname ~ pathashcode ~ instremap ~ temporalExec => new MeetsRequirementsInfPattern(patname, None, Some(pathashcode), None, instremap.get, temporalExec)
  } | "meetsRequirements" ~ ("(" ~> ident) ~ ("," ~> "[" ~> conditionalLRPatternExecList <~ "]") ~ opt("," ~> "[" ~> instMap <~ "]") ~ opt("," ~> execTemporalBlockList) <~ ")" ^^ {
    // Variable-list matching reference
    case "meetsRequirements" ~ patname ~ patCondList ~ None ~ None => new MeetsRequirementsInfPattern(patname, None, None, Some(patCondList), new InstRemap(List()), None)
    case "meetsRequirements" ~ patname ~ patCondList ~ instremap ~ None => new MeetsRequirementsInfPattern(patname, None, None, Some(patCondList), instremap.get, None)
    case "meetsRequirements" ~ patname ~ patCondList ~ None ~ temporalExec => new MeetsRequirementsInfPattern(patname, None, None, Some(patCondList), new InstRemap(List()), temporalExec)
    case "meetsRequirements" ~ patname ~ patCondList ~ instremap ~ temporalExec => new MeetsRequirementsInfPattern(patname, None, None, Some(patCondList), instremap.get, temporalExec)
  }


  // List of instance remappings, e.g. "a -> b, c -> d, e -> f"
  def instMap:Parser[InstRemap] = instMapElem ~ opt(rep("," ~> instMapElem)) ^^ {
    case first ~ None => new InstRemap( first.remappings )
    case first ~ second => {
      val out = new ArrayBuffer[(String, String)]
      for (elem <- second.get) { out.append(elem.remappings(0))}
      new InstRemap( first.remappings ++ out.toList )
    }
  }

  def instMapElem:Parser[InstRemap] = ident ~ "->" ~ ident ^^ {
    case state ~ "->" ~ pattern => new InstRemap(List((state, pattern)))
  }

  def execTemporalBlockList:Parser[ExecTemporalBlockList] = ("temporalblocks" ~ "=" ~ "[") ~> temporalBlockElem ~ opt(rep("," ~> temporalBlockElem)) <~ "]" ^^ {
    case first ~ None => new ExecTemporalBlockList( List(first ) )
    case first ~ second => new ExecTemporalBlockList( List(first) ++ second.get )
  }

  def temporalBlockElem:Parser[ExecTemporalElem] = ident ^^ {
    case a => new ExecTemporalElem(a.toUpperCase())
  }

  def executeAutoPatterns:Parser[ExecuteAutoPatterns] = "executeAutoPatterns" ^^ {
    case _ => new ExecuteAutoPatterns()
  }

  /*
   * Populating inference pattern matches
   */
  def populateInfPatMatches:Parser[PopulateInfPatMatches] = "populateInfPatMatches" ^^ {
    case a => new PopulateInfPatMatches()
  }


  /*
   * Loops
   */
  def whileLoop:Parser[WhileLoop] = ("while" ~ "(" ~> conditionExpr <~ ")") ~ codeblock <~ "endwhile" ^^ {
    case cond ~ code => new WhileLoop(cond, code)
  }

  def break:Parser[Break] = "break" ^^ {
    case _ => new Break()
  }

  /*
   * Hard program stop
   */
  def exit:Parser[Exit] = ("exit" ~ "(" ~> opt(numericLit) <~ ")") ^^ {
    case None => {
      new Exit(0)
    }
    case Some(exitcode) => {
      new Exit(exitcode.toInt)
    }
  }


  /*
   * Verify Answer
   */
  def verifyAnswer:Parser[VerifyAnswer] = ("verifyAnswer" ~ "(" ~> conditionExpr <~ ")") ^^ {
    case cond => new VerifyAnswer(cond)
  }

  /*
   * State control
   */
  def incrementState:Parser[IncrementState] = "incrementState" ^^ {
    case _ => new IncrementState()
  }

  def setStateName:Parser[SetStateName] = "setStateName" ~> "(" ~> stringLit <~ ")" ^^ {
    case name => new SetStateName(name)
  }

  def getStateWithName:Parser[GetStateWithName] = "getStateWithName" ~> ("(" ~> ident) ~ ("," ~> stringLit <~ ")") ^^ {
    case varName ~ name => new GetStateWithName(name, varName)
  }


  /*
   * Conditionals
   */

  def ifStatement:Parser[IfStatement] = conditional ~ codeblock ~ opt(rep(conditionalElseIf)) ~ opt(conditionalElse) <~ "endif" ^^ {
    case ifcond ~ ifcode ~ None ~ None => {
      new IfStatement(List(new IfStatementCondition("IF", Some(ifcond), ifcode)))
    }
    case ifcond ~ ifcode ~ None ~ elseblock => {
      new IfStatement(List(new IfStatementCondition("IF", Some(ifcond), ifcode)) ++ List(elseblock.get))
    }
    case ifcond ~ ifcode ~ elseiflist ~ None => {
      new IfStatement(List(new IfStatementCondition("IF", Some(ifcond), ifcode)) ++ elseiflist.get )
    }
    case ifcond ~ ifcode ~ elseiflist ~ elseblock => {
      new IfStatement(List(new IfStatementCondition("IF", Some(ifcond), ifcode)) ++ elseiflist.get ++ List(elseblock.get))
    }
  }

  //def conditional:Parser[Condition] = "if" ~ "(" ~> (conditionLR | conditionChange) <~ ")" ~ "then" ^^ {
  def conditional:Parser[ConditionExpr] = "if" ~ "(" ~> conditionExpr <~ ")" ~ "then" ^^ {
    case condition => condition
  }

  def conditionalElseIf:Parser[IfStatementCondition] = "else" ~> conditional ~ codeblock ^^ {
    case cond ~ code => new IfStatementCondition("ELSEIF", Some(cond), code)
  }

  def conditionalElse:Parser[IfStatementCondition] = "else" ~> codeblock ^^ {
    case code => new IfStatementCondition("ELSE", None, code)
  }


  // Conditional: Expressions of conditionals

  //------------------------------------------------------------

  def conditionExpr:Parser[ConditionExpr] = conditionFactor ~ rep(("&&" | "||") ~ conditionFactor) ^^ {
    case a ~ List() => a
    case a ~ b => {
      def appendExpression(c:ConditionOperator, p:ConditionOperator):ConditionOperator = {
        p.left = c
        p
      }

      var root:ConditionOperator = new ConditionOperator(b.head._1, a, b.head._2)

      for (f <- b.tail) {
        var parent = f._1 match {
          case "+" => new ConditionOperator("&&", null, f._2)
          case "-" => ConditionOperator("||", null, f._2)
        }

        root = appendExpression(root, parent)
      }

      root
    }

  }

  def conditionFactor:Parser[ConditionExpr] = (conditionLR | conditionChange) ^^ {
    a => new ConditionElem(a)
  } | {
    "(" ~> conditionExpr <~ ")" ^^ { e => e }
  }



  //----------------------

  // Conditional: Typical Left-operator-Right conditionals
  def conditionLR:Parser[Condition] = positioned(expr) ~ ("<" | ">" | "==" | "!=" | "<=" | ">=") ~ positioned(expr) ^^ {
    case left ~ op ~ right => new ConditionLRStr(op, left, right)
  } | "double" ~ "(" ~> positioned(expr) ~ ("<" | ">" | "==" | "!=" | "<=" | ">=") ~ positioned(expr) <~ ")" ^^ {
    case left ~ op ~ right => new ConditionLRDouble(op, left, right)
  }

  /*
  // Conditional: Specific to finding specific patterns that have specific values of internal variables (e.g. <materialName> = obj1."materialName")
  // Used for executing patterns.
  def conditionalLRPatternExec:Parser[Condition] = positioned(multiIdentifier) ~ ("<" | ">" | "==" | "!=" | "<=" | ">=") ~ positioned(expr) ^^ {
    case left ~ op ~ right => new ConditionLRStr(op, left, right)
  }
  */

  /*
  def ifChangeStatement:Parser[IfChangeStatement] = conditionalChange ~ codeblock ~ opt("else" ~> codeblock) <~ "endif" ^^ {
    case cond ~ truecode ~ falsecode => falsecode match {
      case None => new IfChangeStatement(cond, truecode, List())
      case _ => new IfChangeStatement(cond, truecode, falsecode.get)
    }
  }

  // Conditional: Changes
  def conditionalChange:Parser[ConditionChange] = "if" ~ "(" ~> conditionChange <~ ")" ~ "then" ^^ {
    case conditionchange => conditionchange
  }
  */

  def conditionChange:Parser[ConditionChange] = instanceProperty ~ "CHANGE" ~ changeExpr ^^ {
    case instProp ~ "CHANGE" ~ chgexp => new ConditionChange(instProp, chgexp)
  }

  def changeExpr:Parser[Change] = "[" ~> changeElem ~ opt(rep(changeElem)) <~ "]" ^^ {
    case elem1 ~ elems => elems match {
      case None => new Change(List(elem1))
      case _ => new Change(List(elem1) ++ elems.get)
    }
  }

  // TODO: it's not currently clear whether this should be using cellpattern (which essentially matches combinations of stringLiterals and <variables>), or something different.
  def changeElem:Parser[ChangeElem] = ("from" | "to" | "direction" | "threshold") ~ ":" ~ expr ^^ {
    case op ~ ":" ~ ex => new ChangeElem(op, ex)
  }


  /*
   * Temporal blocks
   */
  def temporalBlock:Parser[TemporalBlock] = ("temporalblock" ~ "(") ~> ident ~ (")" ~> codeblock <~ "endtemporalblock") ^^ {    //  "now" | "step" | "eventually"
    case mode ~ codeblock => new TemporalBlock(mode.toUpperCase, codeblock)
  }


  /*
   * Inference Patterns
   */
  // description
  // infInstReq
    // singleInstReq*
    // infConstraint*
  // compInfPatReq
    // compInfPatRef
    // compRowEquiv
    // compInstEquiv
  // infConstraint*
  // constvarInfPattern*
  // rowdef*
  // codeblock

/*
  def infpattern:Parser[InfPattern] = ("inferencepattern" ~> ident) ~ opt(("description" ~> "=") ~> stringLit) ~ opt(infInstReq) ~ opt(compInfPatReq) ~ opt(rep(infConstraint)) ~ opt(rep(constvarInfpattern)) ~ rep(rowdef) ~ opt(codeblock) <~ "endinferencepattern" ^^ {
    case name ~ None ~ None ~ None ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, "", None, None, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ None ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, "", None, None, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ None ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, "", None, None, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ None ~ None ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, None, None, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ None ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, None, None, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ None ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, None, None, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ None ~ None ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, "", None, None, List(), consts.get, rowdefs, code.get)
    case name ~ desc ~ None ~ None ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, None, None, List(), consts.get, rowdefs, code.get)
    case name ~ None ~ infreq ~ None ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, "", infreq, None, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ None ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, "", infreq, None, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ None ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, "", infreq, None, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ infreq ~ None ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, None, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ None ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, None, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ None ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, None, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ infreq ~ None ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, "", infreq, None, List(), consts.get, rowdefs, code.get)
    case name ~ desc ~ infreq ~ None ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, None, List(), consts.get, rowdefs, code.get)
    case name ~ None ~ None ~ compreq ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, "", None, compreq, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ compreq ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, "", None, compreq, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ compreq ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, "", None, compreq, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ None ~ compreq ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, None, compreq, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ compreq ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, None, compreq, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ compreq ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, None, compreq, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ None ~ compreq ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, "", None, compreq, List(), consts.get, rowdefs, code.get)
    case name ~ desc ~ None ~ compreq ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, None, compreq, List(), consts.get, rowdefs, code.get)
    case name ~ None ~ infreq ~ compreq ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, "", infreq, compreq, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ compreq ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, "", infreq, compreq, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ compreq ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, "", infreq, compreq, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ infreq ~ compreq ~ None ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, compreq, List(), List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ compreq ~ None ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, compreq, List(), consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ compreq ~ None ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, compreq, List(), List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ infreq ~ compreq ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, "", infreq, compreq, List(), consts.get, rowdefs, code.get)
    case name ~ desc ~ infreq ~ compreq ~ None ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, compreq, List(), consts.get, rowdefs, code.get)

    case name ~ None ~ None ~ None ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, "", None, None, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ None ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, "", None, None, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ None ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, "", None, None, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ None ~ None ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, None, None, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ None ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, None, None, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ None ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, None, None, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ None ~ None ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, "", None, None, constraints.get, consts.get, rowdefs, code.get)
    case name ~ desc ~ None ~ None ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, None, None, constraints.get, consts.get, rowdefs, code.get)
    case name ~ None ~ infreq ~ None ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, "", infreq, None, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ None ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, "", infreq, None, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ None ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, "", infreq, None, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ infreq ~ None ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, None, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ None ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, None, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ None ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, None, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ infreq ~ None ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, "", infreq, None, constraints.get, consts.get, rowdefs, code.get)
    case name ~ desc ~ infreq ~ None ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, None, constraints.get, consts.get, rowdefs, code.get)
    case name ~ None ~ None ~ compreq ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, "", None, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ compreq ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, "", None, compreq, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ None ~ compreq ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, "", None, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ None ~ compreq ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, None, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ compreq ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, None, compreq, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ None ~ compreq ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, None, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ None ~ compreq ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, "", None, compreq, constraints.get, consts.get, rowdefs, code.get)
    case name ~ desc ~ None ~ compreq ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, None, compreq, constraints.get, consts.get, rowdefs, code.get)
    case name ~ None ~ infreq ~ compreq ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, "", infreq, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ compreq ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, "", infreq, compreq, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ None ~ infreq ~ compreq ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, "", infreq, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ desc ~ infreq ~ compreq ~ constraints ~ None ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ compreq ~ constraints ~ consts ~ rowdefs ~ None => new InfPattern(name, desc.get, infreq, compreq, constraints.get, consts.get, rowdefs, List.empty[Statement])
    case name ~ desc ~ infreq ~ compreq ~ constraints ~ None ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, compreq, constraints.get, List.empty[VariableConstantText], rowdefs, code.get)
    case name ~ None ~ infreq ~ compreq ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, "", infreq, compreq, constraints.get, consts.get, rowdefs, code.get)
    case name ~ desc ~ infreq ~ compreq ~ constraints ~ consts ~ rowdefs ~ code => new InfPattern(name, desc.get, infreq, compreq, constraints.get, consts.get, rowdefs, code.get)

  }
*/


  def infpattern:Parser[InfPattern] = ("inferencepattern" ~> ident) ~ rep(statementInf) ~ opt(codeblock) <~ "endinferencepattern" ^^ {
    case name ~ infstatements ~ None => InfPattern.mkInfPattern(name, infstatements, List.empty[Statement])
    case name ~ infstatements ~ code => InfPattern.mkInfPattern(name, infstatements, code.get)
  }

  def processpattern:Parser[InfPattern] = ("process" ~> ident) ~ ("stage" ~> ident) ~ rep(statementInf) ~ opt(codeblockProc) <~ "endprocess" ^^ {
    case processname ~ processstage ~ infstatements ~ None => InfPattern.mkInfPattern(processname + "_stage_" + processstage, infstatements, List.empty[Statement])
    case processname ~ processstage ~ infstatements ~ code => InfPattern.mkInfPattern(processname + "_stage_" + processstage, infstatements, code.get)
  }

  // description
  // infInstReq
    // singleInstReq*
    // infConstraint*
  // compInfPatReq
    // compInfPatRef
    // compRowEquiv
    // compInstEquiv
  // infConstraint*
  // constvarInfPattern*
  // rowdef*
  // codeblock

  def statementInf:Parser[StatementInf] = positioned(infDescription | patternmatchDescription | infExecuteMode | rowdef | constvarInfpattern | infConstraint |
                                                      compInstEquiv | compRowEquiv | compInfPatRef | compRowEquiv | compInstEquiv |
                                                      infConstraint | instanceRequirement) ^^ {
    a => a
  }


  def statementProc:Parser[Statement] = positioned(processStageTransition | processStageComplete) ^^ {
    a => a
  }


  /*
   * Statements (Process-specific)
   */
  def processStageTransition:Parser[ProcessStageTransition] = ("processStageTransition" ~ "(") ~> ident <~ ")" ^^ {
    case stagename => new ProcessStageTransition(stagename)
  }

  def processStageComplete:Parser[ProcessStageComplete] = ("processStageComplete" ~ "(" ~ ")") ^^ {
    case _ => new ProcessStageComplete()
  }

  /*
   * Statements (Inference Patterns)
   */

  // Description text for an inference pattern
  def infDescription:Parser[InfPatDescription] = ("description" ~> "=") ~> stringLit ^^ {
    case str => new InfPatDescription(str)
  }

  //##
  def patternmatchDescription:Parser[PatternMatchDescription] = ("patterndescription" ~> "=") ~> expr  ^^ {
    case e => new PatternMatchDescription(e)
  }

  // Execution mode for an inference pattern
  def infExecuteMode:Parser[InfPatExecutionMode] = ("executionmode" ~> "=") ~> stringLit ^^ {
    case str => new InfPatExecutionMode(str)
  }

  /*
   * Composite Inference Patterns
   */

  // Composite inference pattern requirements
  def compInfPatReq:Parser[CompInfPatReq] = opt(rep(compInfPatRef)) ~ opt(rep(compRowEquiv)) ~ opt(rep(compInstEquiv)) ^^ {
    case infpatrefs ~ None ~ None => new CompInfPatReq(infpatrefs.get, List(), List())
    case None ~ roweqivs ~ None => new CompInfPatReq(List(), roweqivs.get, List())
    case infpatrefs ~ roweqivs ~ None => new CompInfPatReq(infpatrefs.get, roweqivs.get, List())
    case infpatrefs ~ None ~ instequivs => new CompInfPatReq(infpatrefs.get, List(), instequivs.get)
    case None ~ roweqivs ~ instequivs => new CompInfPatReq(List(), roweqivs.get, instequivs.get)
    case infpatrefs ~ roweqivs ~ instequivs => new CompInfPatReq(infpatrefs.get, roweqivs.get, instequivs.get)
  }

  // Composite inference pattern reference, e.g. infpat x = ChangeOfState
  def compInfPatRef:Parser[CompInfPatRef] = ("infpat" ~> ident) ~ ("=" ~> ident) ^^ {
    case refname ~ patname => new CompInfPatRef(refname, patname)
  }

  // Row equivalency, e.g. rowequiv x.rowname = y.rowname
  def compRowEquiv:Parser[CompRowEquivalency] = ("rowequiv" ~> dotSeparatedIdent) ~ ("=" ~> dotSeparatedIdent) ^^ {
    case ident1 ~ ident2 => new CompRowEquivalency(ident1.name, ident2.name)
  }

  // Prefix for identifiers that include a reference to the state space.  e.g. if (getStateWithName("start").x.y.z == 0) ...
  /*
  def stateRefPrefix:Parser[String] = "getStateWithName" ~> "(" ~> stringLit <~ ")" ^^ {
    case name => name
  }
  */
  def stateRefPrefix:Parser[StateRef] = "getStateWithName" ~> "(" ~> stringLit <~ ")" ^^ {
    case name => new StateWithName(name)
  } | "getStateFirstWithName" ~> "(" ~> stringLit <~ ")" ^^ {
    case name => new StateFirstWithName(name)
  } | "getStateLastWithName" ~> "(" ~> stringLit <~ ")" ^^ {
    case name => new StateLastWithName(name)
  } | "getStateBeforeName" ~> "(" ~> stringLit <~ ")" ^^ {
    case name => new StateBeforeName(name)
  } | "getStateAfterName" ~> "(" ~> stringLit <~ ")" ^^ {
    case name => new StateAfterName(name)
  }

  // getStateWithName, getStateFirstWithName, getStateLastWithName, getStateBeforeName, getStateAfterName

  def dotSeparatedIdent:Parser[Identifier] = opt(stateRefPrefix <~ ".") ~ opt(rep(ident <~ ".")) ~ ident ^^ {
    case stateprefix ~ list ~ last => {
      if ((list.isDefined) && (list.get.length > 0)) {
        new Identifier(list.get.mkString(".") + "." + last, stateprefix) // Multiple elements (e.g. x.y.z.test)
      } else {
        new Identifier(last, stateprefix) // Only a single element (e.g. test)
      }
    }
  }

  // Composite inference pattern reference, e.g. infpat x = ChangeOfState
  def compInstEquiv:Parser[CompInstEquivalency] = ("instmap" ~> ident) ~ ("=" ~> ident) ~ ("." ~> ident) ^^ {
    case instname1 ~ patname2 ~ instname2 => new CompInstEquivalency(instname1, patname2, instname2)
  }


  /*
   * Inference pattern instance requirements
   */
  // inference pattern instance requirements
  def infInstReq:Parser[InfInstanceReq] = instanceRequirement ~ opt(infInstReq) ^^ {
    case instdef ~ None => new InfInstanceReq(List(instdef))
    case instdef ~ next => new InfInstanceReq(List(instdef) ++ next.get.instances)
  }

  // A single instance requirement
  def instanceRequirement:Parser[InstanceRequirement] = ("require" ~> "instance" ~> ident) ~ "=" ~ ("[" ~> "KINDOF" ~> ":" ~> stringLit <~ "]") ^^ {
    case name ~ "=" ~ kindof => new InstanceRequirement(name, kindof)
  }


  // A single inference/instance constriant
  def infConstraint:Parser[InfConstraint] = "shouldhave" ~> ("(" ~> conditionExpr <~ ")") ^^ {
    case condExpr => new InfConstraintShouldHave(condExpr)
  } | "musthave" ~> ("(" ~> conditionExpr <~ ")") ^^ {
    case condExpr => new InfConstraintMustHave(condExpr)
  } | "musthaveoromit" ~> ("(" ~> conditionExpr <~ ")") ^^ {
    case condExpr => new InfConstraintMustHaveOrOmit(condExpr)
  }


  /*
   * Inference pattern main elements
   */
  def constvarInfpattern:Parser[VariableConstantText] = "const" ~> ("<" ~> ident <~ ">") ~ ("=" ~> stringLit) ^^ {
    case name ~ value => new VariableConstantText(name, value)
  }

  /*
  //## TEST
  def rowdef:Parser[RowDef] = ("row" ~> ident) ^^ {
    case name => {
      val cellPat:CellPattern = new CellPattern( List(new CellText("celltext")) )
      new RowDef(name, RowRef(new TableRef("tablename"), List(new CellRef("ColName", cellPat)) ) )
    }
  }*/


  def rowdef:Parser[RowDef] = ("row" ~> opt("*") ~ ident) ~ "=" ~ rowref ^^ {
    case None ~ name ~ "=" ~ rref => new RowDef(name, rref, isOptional = false)
    case optmarker ~ name ~ "=" ~ rref => new RowDef(name, rref, isOptional = true)
  }


  // A row reference is either a tableref or uuidref, plus some number of cellrefs
  def rowref:Parser[RowRef] = "[" ~> (tableref | uuidref) ~ rep("," ~> cellref) <~ "]" ^^ {
    case a ~ cellrefs => a match {
      case table:TableRef => new RowRef(Some(table), None, cellrefs)
      case uuid:UUIDRef => new RowRef(None, Some(uuid), cellrefs)
    }
  }


  /*
  def rowreftableonly:Parser[RowRef] = "[" ~> tableref ~ rep("," ~> cellref) <~ "]" ^^ {
    case table ~ cellrefs => new RowRef(Some(table), None, cellrefs)
  }
  */

  // as above, but only allowing table and not uuid-based references (e.g. for row generation, instead of row lookup)
  def rowreftableonlyExpr:Parser[RowRefExpr] = "[" ~> tableref ~ rep("," ~> cellrefExpr) <~ "]" ^^ {
    case table ~ cellrefs => new RowRefExpr(Some(table), None, cellrefs)
  }


  def tableref:Parser[TableRef] = "TABLE" ~> ":" ~> stringLit ^^ {
    case table => new TableRef(table)
  }

  // Note: singe UUIDs can contain lots of different characters, they are encased in a stringLiteral -- so must be quoted in the script
  def uuidref:Parser[UUIDRef] = "UUID" ~> ":" ~> stringLit ^^ {
    case uuid => new UUIDRef(uuid)
  }


  // Newer: A row reference using Exprs in place of cellrefs, for evaluation within code
  def rowrefExpr:Parser[RowRefExpr] = "[" ~> (tableref | uuidref) ~ rep("," ~> cellrefExpr) <~ "]" ^^ {
    case a ~ cellrefs => a match {
      case table:TableRef => new RowRefExpr(Some(table), None, cellrefs)
      case uuid:UUIDRef => new RowRefExpr(None, Some(uuid), cellrefs)
    }
  }


  /*
   * Cell Patterns
   */

  /*
  // Original
  def cellref:Parser[CellRef] = ident ~ ":" ~ cellpattern ^^ {
    case colname ~ ":" ~ pattern => new CellRef(colname, pattern)
  }
  */

  // Multi-option
  def cellref:Parser[CellRef] = ident ~ ":" ~ cellpattern ~ rep("||" ~> cellpattern) ^^ {
    case colname ~ ":" ~ pattern1 ~ List() => new CellRef(colname, List(pattern1))
    case colname ~ ":" ~ pattern1 ~ patterns2plus => new CellRef(colname, List(pattern1) ++ patterns2plus)
  }

  // Original (using cellPattern)
  def cellpattern:Parser[CellPattern] = cellterm ~ rep("+" ~> cellterm) ^^ {
    case a ~ List() => new CellPattern(List(a))
    case a ~ b => new CellPattern(List(a) ++ b)
  }

  //Variable or double-quote-enclosed string
  // TODO: Does not currently handle the case where both * (optional) and ? (relaxable) markers are specified
  def cellterm:Parser[CellExprElem] = "<" ~> opt("*") ~ opt("~") ~ ident <~ ">" ^^ {
    case None ~ None ~ cellvar => new CellVariable(cellvar, isOptional = false, isRelaxable = false)                // Cell variable name (e.g. <SOM1>)
    case optmarker ~ None ~ cellvar => new CellVariable(cellvar, isOptional = true, isRelaxable = false)            // Cell variable name (e.g. <SOM1>)
    case None ~ relaxmarker ~ cellvar => new CellVariable(cellvar, isOptional = false, isRelaxable = true)            // Cell variable name (e.g. <SOM1>)
    //case optmarker ~ cellvar => new CellVariable("*" + cellvar)     // Cell variable name with optional marker (e.g. <*SOM1>)
  } | opt("*") ~ opt("~") ~ stringLit ^^ {
    case None ~ None ~ str => new CellText(str, isOptional = false, isRelaxable = false)
    case optmarker ~ None ~ str => new CellText(str, isOptional = true, isRelaxable = false)
    case None ~ relaxmarker ~ str => new CellText(str, isOptional = false, isRelaxable = true)
  }

  // Newer (using Expr)
  def cellrefExpr:Parser[CellRefExpr] = ident ~ ":" ~ expr ^^ {
    case colname ~ ":" ~ ex => new CellRefExpr(colname, ex)
  }


  /*
   * Codeblocks
   */

  def codeblock:Parser[List[Statement]] = rep(statement) ^^ {
    a => a
  }

  def codeblockProc:Parser[List[Statement]] = rep(statement | statementProc) ^^ {
    a => a
  }

  // TODO: Minimal
  def statement:Parser[Statement] = positioned(variableAssignment | variableDefinition | instanceDefinition | instancePropAssignment | generateRow | removeRowUUID | removeRowsExpr |
                                                printInstances | println | ifStatement | executePattern | exportInfPatHTML | exportInfPatJSON | exportInfPatDebugHTML | exportTableStoreHTML | exportStateSpaceHTML |
                                                populateInfPatMatches | executeAutoPatterns | temporalBlock | printVariables | addExplanationText |
                                                printState | attachRowToInstance | detachRowFromInstance | whileLoop | break | exit | verifyAnswer |
                                                instantiateInstanceCommonProp | instantiateInstanceWithProp | incrementState | setStateName | getStateWithName |
                                                setEnvironmentVariable) ^^ {
    a => a
  }

  /*
   * Statements (Generate/Remove Rows)
   */
  // Generate a new row
  def generateRow:Parser[GenerateRow] = ("generateRow" ~ "(") ~> rowreftableonlyExpr <~ ")" ^^ {
    case rowrefexpr => new GenerateRow(rowrefexpr, None)
  } | (opt("var") ~> ident) ~ "=" ~ ("generateRow" ~> "(" ~> rowreftableonlyExpr) <~ ")" ^^ {
    case varname ~ "=" ~ rowrefexpr => new GenerateRow(rowrefexpr, Some(varname))
  }

  def removeRowUUID:Parser[RemoveRowUUID] = ("removeRow" ~ "(") ~> expr <~ ")" ^^ {
    case expr => new RemoveRowUUID(expr)
  }

  def removeRowsExpr:Parser[RemoveRowsRowRef] = ("removeRows" ~ "(") ~> rowrefExpr <~ ")" ^^ {
    case ref => new RemoveRowsRowRef(ref)
  }


  /*
   * Statements (printing)
   */
  def println:Parser[Print] = "println" ~ "(" ~> expr <~ ")" ^^ {
    case e => new Print(e)
  }

  def printInstances:Parser[PrintInstances] = "printInstances" ^^ {
    case _ => new PrintInstances()
  }

  def printVariables:Parser[PrintVariables] = "printVariables" ^^ {
    case _ => new PrintVariables()
  }

  def printState:Parser[PrintState] = ("printState" ~ "(" ~ ")") ^^ {
    case _ => new PrintState(includeAutoPatterns = false)
  } | "printState" ^^ {
    case _ => new PrintState(includeAutoPatterns = false)
  } | "printState" ~ "(" ~ "verbose" ~ ")" ^^ {
    case _ => new PrintState(includeAutoPatterns = true)
  }


  def addExplanationText:Parser[AddExplanationText] = ("addExplanationText" ~ "(") ~> expr <~ ")" ^^ {
    case expr => new AddExplanationText(expr)
  }

  /*
   * Statements (Instance definitions)
   */
  def instanceDefinition:Parser[InstanceDefinition] = ("instance" ~> ident) ~ "=" ~ ("[" ~> "KINDOF" ~> ":" ~> expr <~ "]") ^^ {
    case name ~ "=" ~ kindofExpr => new InstanceDefinition(name, kindofExpr)

  }

  //def instancePropAssignment:Parser[InstancePropertyAssignment] = instanceProperty ~ "=" ~ cellpattern ^^ {
  def instancePropAssignment:Parser[InstancePropertyAssignment] = instanceProperty ~ "=" ~ expr ^^ {
    case instprop ~ "=" ~ e => new InstancePropertyAssignment(instprop, e)
  }

  def instanceProperty:Parser[InstanceProperty] = opt(stateRefPrefix <~ ".") ~ (ident <~ ".") ~ stringLit ^^ {
    case stateprefix ~ name ~ propname => new InstanceProperty(name, propname, stateprefix)
  }

  /*
   * Statements (Instantiating instances)
   */
  def instantiateInstanceCommonProp:Parser[InstantiateCommonProperties] = ("instantiate" ~> ident) ~ ("(" ~> "[" ~> "KINDOF" ~> ":" ~> expr <~ "]") <~ ")" ^^ {
    case name ~ kindofExpr => new InstantiateCommonProperties(name, kindofExpr)
  }

  def instantiateInstanceWithProp:Parser[InstantiateWithProperties] = ("instantiate" ~> ident) ~ ("(" ~> "[" ~> "KINDOF" ~> ":" ~> expr <~ "]") ~ ("," ~> "props" ~> "=" ~> stringLit) ~ opt(rep("," ~> stringLit)) <~ ")" ^^ {
    case name ~ kindofExpr ~ prop1 ~ None => new InstantiateWithProperties(name, kindofExpr, List(prop1))
    case name ~ kindofExpr ~ prop1 ~ propsN => new InstantiateWithProperties(name, kindofExpr, List(prop1) ++ propsN.get)
  }

  /*
   * Statements (Attaching table rows to instances)
   */
  def attachRowToInstance:Parser[AttachRowToInstance] = ("attachRowToInstance" ~ "(") ~> ident ~ "," ~ expr <~ ")" ^^ {
    case instname ~ "," ~ e => new AttachRowToInstance(instname, e)
  }

  def detachRowFromInstance:Parser[DetachRowFromInstance] = ("detachRowFromInstance" ~ "(") ~> ident ~ "," ~ expr <~ ")" ^^ {
    case instname ~ "," ~ e => new DetachRowFromInstance(instname, e)
  }

  /*
   * Statements (environment variables)
   */
  def setEnvironmentVariable:Parser[SetEnvironmentVariable] = ("setEnvironmentVariable" ~ "(") ~> stringLit ~ "," ~ stringLit <~ ")" ^^ {
    case varname ~ "," ~ value => new SetEnvironmentVariable(varname, value)
  }

  /*
   * Exporting
   */
  def exportInfPatHTML:Parser[ExportInferencePatternsHTML] = "exportInfPatHTML" ~ "(" ~> stringLit <~ ")" ^^ {
    case filename => new ExportInferencePatternsHTML(filename)
  } | "exportInfPatHTML" ~ "(" ~ ")" ^^ {
    case _ => new ExportInferencePatternsHTML("")   // Use default filename
  }

  def exportInfPatJSON:Parser[ExportInferencePatternsJSON] = "exportInfPatJSON" ~ "(" ~> stringLit <~ ")" ^^ {
    case filename => new ExportInferencePatternsJSON(filename)
  } | "exportInfPatJSON" ~ "(" ~ ")" ^^ {
    case _ => new ExportInferencePatternsJSON("")   // Use default filename
  }

  def exportInfPatDebugHTML:Parser[ExportInferencePatternDebugHTML] = "exportInfPatDebugHTML" ~ "(" ~> stringLit <~ ")" ^^ {
    case path => new ExportInferencePatternDebugHTML(path)
  } | "exportInfPatDebugHTML" ~ "(" ~ ")" ^^ {
    case _ => new ExportInferencePatternDebugHTML("")   // Use default filename
  }

  def exportTableStoreHTML:Parser[ExportTableStoreHTML] = "exportTableStoreHTML" ~ "(" ~> stringLit <~ ")" ^^ {
    case filename => new ExportTableStoreHTML(filename)
  } | "exportTableStoreHTML" ~ "(" ~ ")" ^^ {
    case _ => new ExportTableStoreHTML("")          // Use default filename
  }

  def exportStateSpaceHTML:Parser[ExportStateSpaceHTML] = "exportStateSpaceHTML" ~ "(" ~> stringLit <~ ")" ^^ {
    case filename => new ExportStateSpaceHTML(filename)
  } | "exportStateSpaceHTML" ~ "(" ~ ")" ^^ {
    case _ => new ExportStateSpaceHTML("")          // Use default filename
  }


  /*
   * Variable assignment
   */
  // TODO: Minimal (missing function assignment)
  def variableDefinition:Parser[VariableDefinition] = "var" ~> ident ~ "=" ~ positioned(expr) ^^ {
    case a ~ "=" ~ b => new VariableDefinition(a, b)
  }

  def variableAssignment:Parser[VariableAssignment] = ident ~ "=" ~ positioned(expr) ^^ {
    case a ~ "=" ~ b => new VariableAssignment(a, b)
  }


  // TODO: Minimal
  def expr:Parser[Expr] = term ~ rep(("+" | "-") ~ term) ^^ {
    case a ~ List() => a
    case a ~ b => {
      def appendExpression(c:Operator, p:Operator):Operator = {
        p.left = c
        p
      }

      var root:Operator = new Operator(b.head._1, a, b.head._2)

      for (f <- b.tail) {
        var parent = f._1 match {
          case "+" => new Operator("+", null, f._2)
          case "-" => Operator("-", null, f._2)
        }

        root = appendExpression(root, parent)
      }

      root
    }

  }

  def term:Parser[Expr] = multiplydividemodulo ^^ {
    l => l
  } | factor ^^ {
    a => a
  }

  def multiplydividemodulo:Parser[Expr] = factor ~ rep(("*" | "/" | "%") ~ factor) ^^ {
    case a ~ List() => a
    case a ~ b => {
      def appendExpression(e:Operator, t:Operator):Operator = {
        t.left = e.right
        e.right = t
        t
      }

      var root:Operator = new Operator(b.head._1, a, b.head._2)
      var current = root

      // For each of these possible operators, build the parse tree
      for (f <- b.tail) {
        var rightOperator = f._1 match {
          case "*" => Operator("*", null, f._2)
          case "/" => Operator("/", null, f._2)
          case "%" => Operator("%", null, f._2)
        }
        current = appendExpression(current, rightOperator)
      }

      root
    }
  }

  def factor:Parser[Expr] = multiIdentifier ^^ {
    case multiident => multiident                       // Variable identifiers of all non-instance forms, with optional levels of indirection (e.g. x.y.z.test, <SOM1>, x.y.<SOM1>
  } | instanceProperty ^^ {
    case instProp => instProp                           // Instance property (e.g. object1."state of matter")
  } | stringLit ^^ {
    case str => new Str(str) // String literal (e.g. "text")
  } | numericLit ~ "." ~ numericLit ^^ {
    case a ~ "." ~ b => new Number( (a + "." + b).toDouble )    // Double (e.g. 1.234)
  } | numericLit ^^ {
    case num => new Number(num.toDouble) // Number (e.g. 12)
  } | meetsRequirements ^^ {            // Checking if an inference pattern meets requirements and can be executed
    case mr => mr
  } | {
    "(" ~> expr <~ ")" ^^ { e => e }                    // Any expression wrapped in (brackets)
  }


  // Identifiers that may have some number of degrees of indirection (e.g. x.y.z.<som1>)
  def multiIdentifier:Parser[Expr] = opt(stateRefPrefix <~ ".") ~ opt(rep(ident <~ ".")) ~ ident ^^ {
    case stateprefix ~ list ~ last => {
      if ((list.isDefined) && (list.get.length > 0)) {
        new Identifier(list.get.mkString(".") + "." + last, stateprefix)     // Multiple elements (e.g. x.y.z.test)
      } else {
        new Identifier(last, stateprefix)                                    // Only a single element (e.g. test)
      }
    }
  } | opt(stateRefPrefix <~ ".") ~ opt(rep(ident <~ ".")) ~ cellVarMarker ^^ {
    case stateprefix ~ list ~ last => {
      if ((list.isDefined) && (list.get.length > 0)) {
        new Identifier(list.get.mkString(".") + "." + last.name, stateprefix) // Multiple elements (e.g. x.y.<SOM1>
      } else {
        new Identifier(last.name, stateprefix)                               // Only a single element (e.g. <SOM1>
      }
    }
  }

  // Indentifiers for cell variable markers, e.g. <som1>
  def cellVarMarker:Parser[Identifier] = "<" ~> opt("*") ~ ident <~ ">" ^^ {
    case None ~ cellvar => new Identifier(cellvar, None)              // Cell variable name (e.g. <SOM1>)
    case optmarker ~ cellvar => new Identifier("*" + cellvar, None)   // Cell variable name with optional marker (e.g. <*SOM1>)
  }


  def parseAll[T](p:Parser[T], in:String):ParseResult[T] = {
    phrase(p)(new lexical.Scanner(in))
  }

}
