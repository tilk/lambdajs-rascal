module lambdajs::Reduce

import Option;
import List;
import IO;
import lambdajs::Syntax;
import lambdajs::FreshNames;

opt[Expr] reduce((Expr) `func(<{Id ","}* is>) {<Expr body>} (<{Expr ","}* es>)`) {
  isl = [i | i <- is]; esl = [e | e <- es];
  for (e <- esl) if (!isValue(e)) return none(); 
  try s = (i: e | <i, e> <- zip(isl, esl));
  catch: return none();
  return some(substs(body, s));
}

opt[Expr] reduce((Expr) `if (true) <Expr e1> else <Expr e2>`) = some(e1);
opt[Expr] reduce((Expr) `if (false) <Expr e1> else <Expr e2>`) = some(e2);

opt[Expr] reduce((Expr) `prim ("object?", <Literal l>)`) = some((Expr) `false`);

opt[Expr] reduce((Expr) `prim ("prim-\>num", <Numeric n>)`) = some((Expr) `<Numeric n>`);

opt[Expr] reduce((Expr) `{<Expr e>}`) = some(e);

default opt[Expr] reduce(Expr e) = none();

Expr reduceAll(Expr e) = 
  innermost visit (e) { 
    case Expr e1: {
      switch(reduce(e1)) {
        case some(e2): insert reduceAll(e2);
      }
    }
  };
