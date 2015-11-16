@contributor{
Marek Materzok <tilk@tilk.eu>
}
module lambdajs::Syntax

lexical Whitespace = [\t\n\ \r\f];

lexical Comment
  = @category="Comment" "/*" CommentChar* "*/"
  | @category="Comment" "//" ![\n]*  $
  ;

lexical CommentChar
  = ![*]
  | [*] !>> [/]
  ;

lexical LAYOUT
  = Whitespace
  | Comment
  ;

layout LAYOUTLIST
  = LAYOUT*
  !>> [\t\ \n]
  !>> "/*"
  !>> "//" ;

lexical Int = [a-zA-Z$_0-9] !<< [0-9]+;

syntax Number
  = [a-zA-Z$_0-9] !<< Decimal
  ;

lexical Decimal
  = DecimalInteger [.] [0-9]* ExponentPart?
  | [.] [0-9]+ ExponentPart?
  | DecimalInteger ExponentPart?
  ;

lexical DecimalInteger
  = [0]
  | [1-9][0-9]*
  !>> [0-9]
  ;

lexical ExponentPart
  = [eE] SignedInteger
  ;

lexical SignedInteger
  = [+\-]? [0-9]+ !>> [0-9]
  ;

lexical String
  = [\"] DoubleStringChar* [\"]
  | [\'] SingleStringChar* [\']
  ;
  
lexical DoubleStringChar
  = ![\"\\\n]
  | [\\] EscapeSequence
  ;

lexical SingleStringChar
  = ![\'\\\n]
  | [\\] EscapeSequence
  ;

lexical Id = ([a-zA-Z$_0-9] !<< [$%#_a-zA-Z] [a-zA-Z$_0-9\-]* !>> [a-zA-Z$_0-9\-]) \ Reserved;

keyword Reserved 
  = "NaN"
  | "inf"
  | "undefined"
  | "null"
  | "empty"
  | "true"
  | "false"
  | "func"
  | "let"
  | "rec"
  | "delete"
  | "int"
  | "label"
  | "break"
  | "try"
  | "finally"
  | "throw"
  | "typeof"
  | "prim"
  | "object?"
  | "primitive?"
  | "closure?"
  | "#value"
  | "#writable"
  | "#getter"
  | "#setter"
  | "#enumerable"
  | "#configurable"
  | "#proto"
  | "#code"
  | "#class"
  | "#extensible"
  ;

syntax Bool = "true" | "false";

syntax Numeric =
  | Number
  | "NaN"
  | "inf"
  ;

syntax Literal 
  = numeric: Numeric
  | integer: "int" Int
  | string: String
  | boolean: Bool
  | undefined: "undefined"
  | empty: "empty"
  | null: "null"
  ;

syntax Oattr 
  = "#proto"
  | "#code"
  | "#class"
  | "#extensible"
  ;

syntax Pattr
  = "#value"
  | "#writable"
  | "#getter"
  | "#setter"
  | "#enumerable"
  | "#configurable"
  ;

syntax OattrDef = oattrDef: Oattr ":" Expr | iattrDef: Id ":" Expr;

syntax PattrDef = pattrDef: Pattr Expr; 

syntax AttrName = String | Id;

syntax Attr = AttrName ":" "{" {PattrDef ","}* "}"; 

syntax Expr 
  = literal : Literal
  | id: Id
  | bracket paren: "(" Expr ")"
  | bracket brkt: "{" Expr "}"
  | obj: "{" "[" {OattrDef ","}* "]" {Attr ","}* "}"
  | func: "func" "(" {Id ","}* ")" "{" Expr "}"
  | failure: "fail" "(" String ")"
  | primUnary: "prim" "(" String "," Expr ")"
  | primBinary: "prim" "(" String "," Expr "," Expr ")"
  | eval: "@eval" "(" Expr "," Expr ")"
  > typeof: "typeof" Expr
  | not: "!" Expr
  | isobject: "object?" Expr
  | isprimitive: "primitive?" Expr
  | isclosure: "closure?" Expr
  | app: Expr "(" {Expr ","}* ")"
  | let: "let" "(" Id "=" Expr ")" Expr
  | rec: "rec" "(" Id "=" Expr ")" Expr
  | ifThenElse: "if" "(" Expr ")" Expr "else" Expr
  | ifThen: "if" "(" Expr ")" Expr !>> "else"
  | attrGet: Expr "[" Expr "\<" Pattr "\>" "]"
  | attrSet: Expr "[" Expr "\<" Pattr "\>" "=" Expr "]"
  | attrDel: Expr "[" "delete" Expr "]"
  | oattrGet: Expr "[" "\<" Oattr "\>" "]"
  | oattrSet: Expr "[" "\<" Oattr "\>" "=" Expr "]"
  | iattrGet: Expr "[" "\<" Id "\>" "]"
  | iattrSet: Expr "[" "\<" Id "\>" "=" Expr "]"
  > left 
    ( mul : Expr "*" Expr
    | div : Expr "/" Expr)
  > left 
    ( add : Expr "+" Expr
    | sub : Expr "-" Expr)
  > non-assoc 
    ( eq: Expr "==" Expr
    | neq: Expr "!=" Expr
    | sv: Expr "===" Expr
    | nsv: Expr "!==" Expr)
  > left and: Expr "&&" Expr
  | left or: Expr "||" Expr
  > throwEx: "throw" Expr
  | breakEx: "break" Id Expr
  | tryCatch: "try" Expr "catch" Expr
  | tryFinally: "try" Expr "finally" Expr
  | label: "label" Id ":" Expr
  > right
    ( fseq: Expr ";" Expr
    | seq: Expr ";;" Expr)
  ;

syntax EnvDef 
  = env_let: "let" "[" Id "]" "=" Expr
  | env_rec: "rec" "[" Id "]" "=" Expr
  | env_block: "{" Expr "}"
  ;

start syntax Prog = prog: Expr;

start syntax Env = env: EnvDef*;

