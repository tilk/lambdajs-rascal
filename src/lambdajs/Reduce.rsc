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

opt[Expr] reduce((Expr) `let (<Id i> = <Value e1>) <Expr e2>`) = some(subst(e2, i, (Expr)`<Value e1>`));

opt[Expr] reduce((Expr) `if (true) <Expr e1> else <Expr e2>`) = some(e1);
opt[Expr] reduce((Expr) `if (false) <Expr e1> else <Expr e2>`) = some(e2);

opt[Expr] reduce((Expr) `<Value e1>; <Expr e2>`) = some(e2); 

opt[Expr] reduce((Expr) `<Value e1>;; <Value e2>`) = some(e2);
opt[Expr] reduce((Expr) `empty;; <Expr e>`) = some(e);
opt[Expr] reduce((Expr) `<Expr e>;; empty`) = some(e);

opt[Expr] reduce((Expr) `try <Expr e1> finally <Value e2>`) = some(e1);
opt[Expr] reduce((Expr) `try <Value e1> finally <Expr e2>`) = some((Expr) `<Expr e2>; <Value e1>`);

opt[Expr] reduce((Expr) `try <Value e1> catch <Expr e2>`) = some(e1);

opt[Expr] reduce((Expr) `prim ("object?", <Literal l>)`) = some((Expr) `false`);
opt[Expr] reduce((Expr) `prim ("object?", <Func f>)`) = some((Expr) `false`);

opt[Expr] reduce((Expr) `prim ("prim-\>num", <Numeric n>)`) = some((Expr) `<Numeric n>`);

opt[Expr] reduce((Expr) `{<Expr e>}`) = some(e); // TODO can brackets be ignored in matching?

opt[Expr] reduce((Expr) `prim ("typeof", <Numeric n>)`) = some((Expr) `"number"`);
opt[Expr] reduce((Expr) `prim ("typeof", <Func n>)`) = some((Expr) `"function"`);
opt[Expr] reduce((Expr) `prim ("typeof", <String s>)`) = some((Expr) `"string"`);
opt[Expr] reduce((Expr) `prim ("typeof", <Bool b>)`) = some((Expr) `"boolean"`);
opt[Expr] reduce((Expr) `prim ("typeof", undefined)`) = some((Expr) `"undefined"`);
opt[Expr] reduce((Expr) `prim ("typeof", null)`) = some((Expr) `"null"`);

opt[Expr] reduce((Expr) `prim ("==", <String s1>, <String s2>)`) {
  b = mkBool(stringValue(s1) == stringValue(s2));
  return some((Expr) `<Bool b>`);
}

opt[Expr] reduce((Expr) `prim ("==", <Bool b1>, <Bool b2>)`) {
  b = mkBool(boolValue(b1) == boolValue(b2));
  return some((Expr) `<Bool b>`);
}

opt[Expr] reduce((Expr) `prim ("==", undefined, undefined)`) = some((Expr) `true`);
opt[Expr] reduce((Expr) `prim ("==", null, null)`) = some((Expr) `true`);
opt[Expr] reduce((Expr) `prim ("==", empty, empty)`) = some((Expr) `true`);

opt[Expr] reduce((Expr) `prim ("===", <String s1>, <String s2>)`) = mkBool(stringValue(s1) == stringValue(s2));
opt[Expr] reduce((Expr) `prim ("===", <Bool b1>, <Bool b2>)`) = mkBool(boolValue(b1) == boolValue(b2));
opt[Expr] reduce((Expr) `prim ("===", undefined, undefined)`) = some((Expr) `true`);
opt[Expr] reduce((Expr) `prim ("===", null, null)`) = some((Expr) `true`);
opt[Expr] reduce((Expr) `prim ("===", empty, empty)`) = some((Expr) `true`);

default opt[Expr] reduce(Expr e) = none();

Expr reduceAll(Expr e) = 
  innermost visit (e) { 
    case Expr e1: {
      switch(reduce(e1)) {
        case some(e2): insert reduceAll(e2);
      }
    }
  };
