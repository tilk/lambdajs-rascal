module lambdajs::Env

import lambdajs::Syntax;
import ParseTree;
import IO;

public start[Env] parseEnv(str txt) = parse(#start[Env], txt);

public start[Env] parseEnv(loc fname) = parse(#start[Env], fname);

public map[Id, Expr] letFuncs(start[Env] env) {
  map[Id, Expr] values = ();
  visit(env) { case (EnvDef) `let [<Id i>] = <Func f>`: values[i] = (Expr)`<Func f>`;  };
  return values;
}
