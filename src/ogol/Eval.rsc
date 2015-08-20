module ogol::Eval

import ogol::Syntax;
import ogol::Canvas;
import ogol::Canvas2JS;
import util::Math;
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

//compileCanvas(eval((Program)`pendown;left 30; right 60; left 30;`), l)

//compileCanvas(eval((Program)`pendown;left 30;`), |file:///Users/manenvana/Documents/workspace/ogol/input/test.test|)

//Program prog = (Program)`home;`;
//eval((Program)`home;`);
// Top-level eval function
Canvas eval(Program p){
		println("desugar");
		p = desugar(p);
		return eval(p);
}

void main(){
	Program p = (Program)`pendown; repeat 8 [ forward 40; right 90; ] forward 100; right 279; forward 20; to FuncD :n :len pd; end`;
	p = desugar(p); //Werkt nog niet
	Canvas cv = eval(p);
	compileCanvas(cv, |file:///Users/manenvana/Documents/workspace/ogol/input/test.test|);
}

Canvas eval(p: (Program) `<Command* cmds>`){
	
	FunEnv funenv = collectFunDefs(p);
	VarEnv varEnv = ();
	println(funenv[(FunId)`FuncD`]);

	int direction = 0;
	bool pendown = false;
	Point positionHome = <0,0>;
	Turtle t = <direction, pendown, positionHome>;
	Canvas canvas = [];
	State state = <t,canvas>;
	

	for (c <- cmds) {
		state = eval(c, funenv, varEnv, state);
	}
//	println(state);
	return state.canvas;
}

FunEnv collectFunDefs(Program p)  = ( f.id: f | /FunDef f := p );



State eval(Command cmd, FunEnv fenv, VarEnv venv, State state){


	if (cmd == (Command)`home;`){
		 state.turtle.position.x=0; 
		 state.turtle.position.y=0;
	} elseif (cmd == (Command)`penup;`){
		 state.turtle.pendown =false; 
	} elseif (cmd == (Command)`pendown;`){
		 state.turtle.pendown =true; 	
	} else {
		println("none of the above");
	}
	return state;
}

State eval((Command)`right <Expr x>;`, FunEnv fenv, VarEnv venv, State state) = turn(state, eval(x, venv));
State eval((Command)`left  <Expr x>;`, FunEnv fenv, VarEnv venv, State state) = turn(state, eval(x, venv)); //doet het niet
State eval((Command)`forward <Expr x>;`, FunEnv fenv, VarEnv venv, State state) = setStep(state, eval(x, venv));
State eval((Command)`back <Expr x>;`, FunEnv fenv, VarEnv venv, State state) = setStep(state, eval(x, venv));


State eval((Command)`repeat  <Expr e>  [ <Command* cmds> ]`, FunEnv fenv, VarEnv venv, State state) {
	println("repeat");
	int count = eval(e, venv);
	for(int i <- [1..round(count)+1]) {
		for (c <- cmds) {
    		state = eval(c, fenv, venv, state);
    	}
	}
	return state;
}
 


State turn(State state, int direction){
	state.turtle.dir = state.turtle.dir +direction;
	return state;
}


State setStep(State state, int length){
	Point p1 = <state.turtle.position.x,state.turtle.position.y>;
	Point p2 = getNewPoint(p1, state.turtle.dir, length);
	state.turtle.position = p2;
	if(state.turtle.pendown == true){	
		Shape shape = line(p1,p2);
		state.canvas = state.canvas + shape;
	}
	return state;
}

Point getNewPoint(Point p, int direction, int distance){
		real angle = direction * PI() / 180;
		p.x   = toInt(p.x + distance * cos(angle));
		p.y   = toInt(p.y + distance * sin(angle));
		return p;
}

test bool testTrue() = eval((Command)`home;`, (), (), <<0,false,<0,0>>,[]>)
	== <<0,false,<0,0>>,[]>;


Value eval(Expr e, VarEnv venv){
	switch(e){
		case (Expr)`true` : return boolean(true);
		case (Expr)`false` : return boolean(false);	
		case (Expr)`<VarId n>` : return venv[n];	
	}
}
int eval((Expr)`<Number e>`, VarEnv venv){
	return toInt(unparse(e));
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
	return p;
}
