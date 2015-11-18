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
  | "fail"
  | "object?"
  | "primitive?"
  | "closure?"
  | "get-own-field-names"
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

syntax Numeric 
  = Number
  | "NaN"
  | "+inf"
  | "-inf"
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

syntax Func = "func" "(" {Id ","}* is ")" "{" Expr e "}";

syntax Expr 
  = literal : Literal lit
  | id: Id i
  | bracket paren: "(" Expr e ")"
  | bracket brkt: "{" Expr e "}"
  | obj: "{" "[" {OattrDef ","}* "]" {Attr ","}* "}"
  | func: Func f
  | failure: "fail" "(" String s ")"
  | unary: "prim" "(" String n "," Expr e ")"
  | binary: "prim" "(" String n "," Expr e1 "," Expr e2 ")"
  | eval: "@eval" "(" Expr e1 "," Expr e2 ")"
  | gofn: "get-own-field-names" "(" Expr e ")"
  > typeof: "typeof" Expr e
  | not: "!" Expr e
  | neg: "-" Expr e
  | isobject: "object?" Expr e
  | isprimitive: "primitive?" Expr e
  | isclosure: "closure?" Expr e
  | app: Expr e "(" {Expr ","}* es ")"
  | ifThenElse: "if" "(" Expr e ")" Expr e1 "else" Expr e2
  | ifThen: "if" "(" Expr e ")" Expr e1 !>> "else"
  | attrGet: Expr e "[" Expr e1 "\<" Pattr pa "\>" "]"
  | attrSet: Expr e "[" Expr e1 "\<" Pattr pa "\>" "=" Expr "]"
  | attrDel: Expr e "[" "delete" Expr e1 "]"
  | oattrGet: Expr e "[" "\<" Oattr oa "\>" "]"
  | oattrSet: Expr e "[" "\<" Oattr oa "\>" "=" Expr e1 "]"
  | iattrGet: Expr e "[" "\<" Id i "\>" "]"
  > left 
    ( mul : Expr e1 "*" Expr e2
    | div : Expr e1 "/" Expr e2)
  > left 
    ( add : Expr e1 "+" Expr e2
    | sub : Expr e1 "-" Expr e2)
  > non-assoc 
    ( eq: Expr e1 "==" Expr e2
    | neq: Expr e1 "!=" Expr e2
    | sv: Expr e1 "===" Expr e2
    | nsv: Expr e1 "!==" Expr e2)
  > left and: Expr e1 "&&" Expr e2
  | left or: Expr e1 "||" Expr e2
  > throwEx: "throw" Expr e
  | breakEx: "break" Id i Expr e
  | tryCatch: "try" Expr e1 "catch" Expr e2
  | tryFinally: "try" Expr e1 "finally" Expr e2
  | label: "label" Id i ":" Expr e
  > let: "let" "(" Id i "=" Expr e1 ")" Expr e2
  | rec: "rec" "(" Id i "=" Expr e1 ")" Expr e2
  > right
    ( fseq: Expr e1 ";" Expr e2
    | seq: Expr e1 ";;" Expr e2)
  ;

syntax EnvDef 
  = env_let: "let" "[" Id i "]" "=" Expr e
  | env_rec: "rec" "[" Id i "]" "=" Expr e
  | env_block: "{" Expr e "}"
  ;

start syntax Prog = prog: Expr e;

start syntax Env = env: EnvDef*;

bool isValue((Expr)`<Literal l>`) = true;
bool isValue((Expr)`<Id id>`) = true;
bool isValue((Expr)`func (<{Id ","}* ids>) { <Expr e> }`) = true;
default bool isValue(Expr e) = false;

