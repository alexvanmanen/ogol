module ogol::CallGraph
import ParseTree;
import ogol::Syntax;
import analysis::graphs::Graph;
import Vis::Figure::Figures::graph;
import IO;
alias graph[&T] = rel[&T, &T];


graph[str] extractor(){
	Program p = parse(#start[Program], |project://Ogol/input/dashed.ogol|).top;
	graph[str] graaf = funcsUsedInCommands("/",p.commands);
	
	//render(graph(nodes, edges, hint("layered"), gap(100)));
	
	set[str] entryPoints = top(graaf);
	println("startpunten: <entryPoints>" );
	println("startpunten: " + entryPoints);

	return graaf;
}

graph[str] funcsUsedInCommands(str scope, Command* commands) = { *funcsUsedInCommand(scope, cmd) | cmd <- commands };


graph[str] funcsUsedInCommand(str scope, (Command)`to <FunId fid> <VarId* args> <Command* commands> end`){
	graph[str] partGraph = {};
	for (c <- commands) {
		partGraph += funcsUsedInCommand("<fid>", c);
	}
	
	return partGraph;
}

graph[str] funcsUsedInCommand(str scope, (Command)`<FunId fid> <Expr* args>;`){
	//println(fid);
	return {<"<scope>", "<fid>">};
}

graph[str] funcsUsedInCommand(str scope, (Command)`repeat  <Expr x>   [<Command* cmds>]`){
	//println(scope);
	return funcsUsedInCommands(scope, cmds);
}

default graph[str]  funcsUsedInCommand(str scope, Command cmd){
	return {};
}


