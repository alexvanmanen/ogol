module ogol::Eval

import ogol::Syntax;
import ogol::Canvas;
import Prelude;

alias FunEnv = map[FunId id, FunDef def];

alias VarEnv = map[VarId id, Value val];

data Value
  = boolean(bool b)
  | number(real i)
  ;

/*
         +y
         |
         |
         |
-x ------+------- +x
         |
         |
         |
        -y

NB: home = (0, 0)
*/



alias Turtle = tuple[int dir, bool pendown, Point position];

alias State = tuple[Turtle turtle, Canvas canvas];


//Program prog = (Program)`home;`;
//eval((Program)`home;`);
// Top-level eval function
Canvas eval((Program) `<Command* cmds>`){
	//funenv = collectFunDefs(p);
	FunEnv funenv =();
	VarEnv varEnv =();
	int direction = 0;
	bool pendown = false;
	Point positionHome = <0,0>;
	Turtle t = <direction, pendown, positionHome>;
	Canvas canvas = [];
	State state = <t,canvas>;
//	//state = evalCommands((Command)`home;`, (), (), <<0,false,<0,0>>,[]>)
		

	for (c <- cmds) {
		state = eval(c, funenv, varEnv, state);
	}
	return canvas;
}



FunEnv collectFunDefs(Program p){

	
}


//evalCommands((Command)`home;`, (), (), <<0,false,<0,0>>,[]>);
State eval(Command cmd, FunEnv fenv, VarEnv venv, State state){


	if (cmd == (Command)`home;`){
		 state.turtle.position.x=0; 
		 state.turtle.position.y=0;
	} elseif (cmd == (Command)`penup;`){
		 state.turtle.pendown =false; 
	} elseif (cmd == (Command)`pendown;`){
		 state.turtle.pendown =true; 
	} elseif (cmd == (Command)`left  1;`){
		 println("haha");
		  
	} elseif (cmd == (Command)`left <Num venv>;`){
		 println("hihi");
		  
	} else{
		println("none of the above");
	}
	
	return state;
}

test bool testTrue() = eval((Command)`home;`, (), (), <<0,false,<0,0>>,[]>)
	== <<0,false,<0,0>>,[]>;


Value eval(Expr e, VarEnv venv){
	switch(e){
		case (Expr)`true` : return boolean(true);
		case (Expr)`false` : return boolean(false);
		case (Expr)`<Num n>` : return number(toReal(unparse(n)));	
		case (Expr)`<VarId n>` : return venv[n];

		
	}
}

Value eval((Expr) `<Expr lhs> + <Expr rhs>`, VarEnv venv)= 
		number(x+y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);


Value eval((Expr) `<Expr lhs> - <Expr rhs>`, VarEnv venv)= 
		number(x-y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);


Value eval((Expr) `<Expr lhs> * <Expr rhs>`, VarEnv venv)= 
		number(x*y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);

Value eval((Expr) `<Expr lhs> / <Expr rhs>`, VarEnv venv)= 
		number(x/y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);


		 
Value eval((Expr) `<Expr lhs> + <Expr rhs>`, VarEnv venv)= 
		number(x+y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);


Value eval((Expr) `<Expr lhs> - <Expr rhs>`, VarEnv venv)= 
		number(x-y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);


Value eval((Expr) `<Expr lhs> * <Expr rhs>`, VarEnv venv)= 
		number(x*y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);

Value eval((Expr) `<Expr lhs> / <Expr rhs>`, VarEnv venv)= 
		number(x/y)
		 when number(x) := eval(lhs, venv),
		 number(y) := eval(rhs, venv);


		 
Value eval((Expr) `<Expr lhs> \> <Expr rhs>`, VarEnv venv) = 
	boolean(x > y) 
	when number(x) := eval(lhs, venv), 
	number(y) := eval(rhs, venv);
Value eval((Expr) `<Expr lhs> \< <Expr rhs>`, VarEnv venv) = 
	boolean(x < y) 
	when number(x) := eval(lhs, venv), 
	number(y) := eval(rhs, venv);

Value eval((Expr) `<Expr lhs> \>= <Expr rhs>`, VarEnv venv) = 
	boolean(x >= y) 
	when number(x) := eval(lhs, venv), 
	number(y) := eval(rhs, venv);

Value eval((Expr) `<Expr lhs> \<= <Expr rhs>`, VarEnv venv) = 
	boolean(x <= y) 
	when number(x) := eval(lhs, venv), 
	number(y) := eval(rhs, venv);

Value eval((Expr) `<Expr lhs> = <Expr rhs>`, VarEnv venv) = boolean(lhs == rhs);

Value eval((Expr) `<Expr lhs> != <Expr rhs>`, VarEnv venv) = boolean(lhs != rhs);


Value eval((Expr) `<Expr lhs> && <Expr rhs>`, VarEnv venv) = 
	boolean(x && y) 
	when number(x) := eval(lhs, venv), 
	number(y) := eval(rhs, venv);


Value eval((Expr) `<Expr lhs> || <Expr rhs>`, VarEnv venv) = 
	boolean(x || y) 
	when number(x) := eval(lhs, venv), 
	number(y) := eval(rhs, venv);
		



default Value eval(Expr e, VarEnv venv){
	return "doesnt work";
}

test bool testTrue() = eval((Expr)`true`,()) == boolean(true);
test bool testFalse() = eval((Expr)`false`,()) == boolean(false);
test bool testNum1() = eval((Expr)`1.0`,()) == number(1.0);
test bool testNum2() = eval((Expr)`-81.2`,()) == number(-81.2);
test bool testVar1() = eval((Expr)`:n`,((VarId)`:n`: number(1.0))) == number(1.0);
test bool testVar2() = eval((Expr)`:n`,((VarId)`:n`: boolean(true))) == boolean(true);
test bool testMul() = eval((Expr)`1.0 * 2.0`,()) == number(2.00);
test bool testDiv() = eval((Expr)`1.0 / 2.0`,()) == number(0.5);
test bool testPlus() = eval((Expr)`1.0 + 2.0`,()) == number(3.0);
test bool testMin() = eval((Expr)`1.0 - 2.0`,()) == number(-1.0);


test bool testEqual1() = eval((Expr)`true = true`,()) == boolean(true);
test bool testEqual2() = eval((Expr)`true = false`,()) == boolean(false);
test bool testEqual3() = eval((Expr)`1 = 2`,()) == boolean(false);
test bool testEqual4() = eval((Expr)`1 = 1`,()) == boolean(true);
test bool testSmall1() = eval((Expr)`1 \> 2`,()) == boolean(false);
test bool testSmall2() = eval((Expr)`2 \> 1`,()) == boolean(true);



Program desugar(Program p){
	visit(p){
	 	case (Command)`fd <Expr e>;` => (Command)`forward <Expr e> ;`
  		case (Command)`bk <Expr e>;` => (Command)`back <Expr e> ;`
  		case (Command)`rt <Expr e>;` => (Command)`right <Expr e> ;`
    	case (Command)`lt <Expr e>;` => (Command)`left <Expr e> ;`
    	case (Command)`pu;` => (Command)`penup;`
    	case (Command)`pd;` => (Command)`pendown;`
		};
}
