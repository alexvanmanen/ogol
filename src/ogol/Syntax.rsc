module ogol::Syntax

import Prelude;
import vis::ParseTree;

/*

Ogol syntax summary

Program: Command...

Command:
 * Control flow: 
  if Expr Block
  ifelse Expr Block Block
  while Expr Block
  repeat Expr Block
 * Drawing (mind the closing semicolons)
  forward Expr; fd Expr; back Expr; bk Expr; home;
  right Expr; rt Expr; left Expr; lt Expr; 
  pendown; pd; penup; pu;
 * Procedures
  definition: to Name [Var...] Command... end
  call: Name Expr... ;
 
Block: [Command...]
 
Expressions
 * Variables :x, :y, :angle, etc.
 * Number: 1, 2, -3, 0.7, -.1, etc.
 * Boolean: true, false
 * Arithmetic: +, *, /, -
 * Comparison: >, <, >=, <=, =, !=
 * Logical: &&, ||

Reserved keywords
 if, ifelse, while, repeat, forward, back, right, left, pendown, 
 penup, to, true, false, end

Bonus:
 - add literal for colors
 - support setpencolor

*/

start syntax Program = Command*; 



lexical Bool = "true" |  "false";
lexical Number = "."? [0-9]+ !>> [0-9];
syntax Expr = Bool | Number| VarId | 
			  Expr "+"  Expr |
			  Expr "*"  Expr |
			  Expr "/"  Expr |
			  Expr "-"  Expr |
			  Expr "\>"  Expr |
			  Expr "\<"  Expr |
			  Expr "\>="  Expr |
			  Expr "\<="  Expr |
			  Expr "="   Expr |
			  Expr "!="  Expr |
			  Expr "&&"  Expr |
			  Expr "||"  Expr;



syntax Command = ControlFlow | Procedures; 
syntax Drawing = "forward" Expr ";" | "fd" Expr ";" |
				 "back" Expr ";" | "bk" Expr ";" |
				 "home;" | "right" Expr ";" | "rt" Expr ";" |
				 "left" Expr ";" | "lt" Expr ";" | "pendown;" |
				 "pd;" | "penup;" | "pu;";

syntax Procedures = left FunDef | FunCall;
syntax FunCall = FunId Expr+ ";";
syntax FunDef = "to " FunId VarId* Command* "end"; 

syntax ControlFlow = "if"  Expr   Block 
					|"ifelse"  Expr   Block  Block
					|"while"  Expr   Block ;

syntax Block = "[" Command* "]";

lexical Keywords = "if" | "ifelse" | "while" | "repeat" | "forward" | "back" | "right" 
					| "left" | "pendown" | "penup" | "to" | "true" | "false" | "end";
 
lexical VarId = ":" [a-zA-Z][a-zA-Z0-9]* !>> [a-zA-Z0-9];  
lexical FunId = [a-zA-Z][a-zA-Z0-9]* !>> [a-zA-Z0-9];

layout Standard = WhitespaceOrComment* !>> [\ \t\n\r] !>> "--";

lexical WhitespaceOrComment = whitespace: Whitespace | comment: Comment; 
lexical Whitespace = [\ \t\n\r];
lexical Comment = @category="Comment" "--" ![\n\r]* $;  
  

  
  public bool testProgram(str text){
  	try { 
	  	
	  	return /amb(_)!:= parse(#start[Program], text);
	  	
  	} 
  	catch x: return false;
  	
  }
    public bool testC(c, str text){
  	try { 
	  	
	  	return /amb(_)!:= parse(c, text);
	  	
  	} 
  	catch x: return false;
  	
  }
  
  public test bool t2198() = testC(#Expr,"true");
  public test bool t2198() = testC(#Expr,"8");
  
  public test bool t01() = testProgram("ifelse 8 [fd :size;] [if 8 [fd :size;]]");
  public test bool t02() = testProgram("fd :size;");
  
  public test bool t03() = testProgram("if true [fd :size;]");
  
  public test bool t04() = testProgram("to squareDash :n :len fd :size; end");
  void vt04() = renderParsetree(parse(#start[Program], "to squareDash :n :len fd :size; end"));
  public test bool t04() = testProgram(readFile(|file:///Users/manenvana/Documents/workspace/ogol/input/test.ogol|));
  public test bool t05() = testProgram("to tree :size if :size = 5 []end");
  public test bool t06() = testProgram(readFile(|file:///Users/manenvana/Documents/workspace/ogol/input/trees.ogol|));
  public test bool t07() = testProgram(readFile(|file:///Users/manenvana/Documents/workspace/ogol/input/dashed.ogol|));
  public test bool t08() = testProgram(readFile(|file:///Users/manenvana/Documents/workspace/ogol/input/octagon.ogol|));
	
  
  