module lambdajs::Reduce

import Option;
import List;
import IO;
import lambdajs::Syntax;
import lambdajs::FreshNames;
import lambdajs::FloatingPoint;

opt[Expr] reduce(subst sub, e0: (Expr) `<Id i> (<{Expr ","}* es>)`) {
  for (e <- es) if (!isValue(e)) return none();
  if ((true | it && isId(e) | e <- es)) return none();
  switch (sub(i)) {
    case none(): return none();
    case some(e1): return some(e0[e=e1]);
  }
}

opt[Expr] reduce(subst sub, (Expr) `func(<{Id ","}* is>) {<Expr body>} (<{Expr ","}* es>)`) {
  isl = [i | i <- is]; esl = [e | e <- es];
  for (e <- esl) if (!isValue(e)) return none(); 
  try s = (i: e | <i, e> <- zip(isl, esl));
  catch: return none();
  return some(substs(body, mkSubst(s)));
}

opt[Expr] reduce(subst sub, (Expr) `let (<Id i> = <Value e1>) <Expr e2>`) = some(subst(e2, i, (Expr)`<Value e1>`));

opt[Expr] reduce(subst sub, (Expr) `if (true) <Expr e1> else <Expr e2>`) = some(e1);
opt[Expr] reduce(subst sub, (Expr) `if (false) <Expr e1> else <Expr e2>`) = some(e2);

opt[Expr] reduce(subst sub, (Expr) `<Value e1>; <Expr e2>`) = some(e2); 

opt[Expr] reduce(subst sub, (Expr) `<Value e1>;; <Value e2>`) = some(e2);
opt[Expr] reduce(subst sub, (Expr) `empty;; <Expr e>`) = some(e);
opt[Expr] reduce(subst sub, (Expr) `<Expr e>;; empty`) = some(e);

opt[Expr] reduce(subst sub, (Expr) `try <Expr e1> finally <Value e2>`) = some(e1);
opt[Expr] reduce(subst sub, (Expr) `try <Value e1> finally <Expr e2>`) = some((Expr) `<Expr e2>; <Value e1>`);

opt[Expr] reduce(subst sub, (Expr) `try <Value e1> catch <Expr e2>`) = some(e1);

opt[Expr] reduce(subst sub, (Expr) `{<Expr e>}`) = some(e); // TODO can brackets be ignored in matching?

opt[Expr] reduce(subst sub, (Expr) `prim(<String s>, <Expr e>)`) = reduceUnary(stringValue(s), e);

opt[Expr] reduce(subst sub, (Expr) `prim(<String s>, <Expr e1>, <Expr e2>)`) = reduceBinary(stringValue(s), e1, e2);

default opt[Expr] reduce(subst sub, Expr e) = none();

opt[Expr] reduceBinary("===", (Expr) `<String s1>`, (Expr) `<String s2>`) = 
  some(mkBoolExpr(stringValue(s1) == stringValue(s2)));
opt[Expr] reduceBinary("===", (Expr) `<Bool b1>`, (Expr) `<Bool b2>`) = 
  some(mkBoolExpr(boolValue(b1) == boolValue(b2)));
opt[Expr] reduceBinary("===", (Expr) `undefined`, (Expr) `undefined`) = some((Expr) `true`);
opt[Expr] reduceBinary("===", (Expr) `null`, (Expr) `null`) = some((Expr) `true`);
opt[Expr] reduceBinary("===", (Expr) `empty`, (Expr) `empty`) = some((Expr) `true`);

opt[Expr] reduceBinary("==", (Expr) `<String s1>`, (Expr) `<String s2>`) = 
  some(mkBoolExpr(stringValue(s1) == stringValue(s2)));
opt[Expr] reduceBinary("==", (Expr) `<Bool b1>`, (Expr) `<Bool b2>`) = 
  some(mkBoolExpr(boolValue(b1) == boolValue(b2)));
opt[Expr] reduceBinary("==", (Expr) `undefined`, (Expr) `undefined`) = some((Expr) `true`);
opt[Expr] reduceBinary("==", (Expr) `null`, (Expr) `null`) = some((Expr) `true`);
opt[Expr] reduceBinary("==", (Expr) `mpty`, (Expr) `empty`) = some((Expr) `true`);

opt[Expr] reduceBinary("+", (Expr) `<Numeric n1>`, (Expr) `<Numeric n2>`) = 
  some(mkNumericExpr(addFP(numericValue(n1), numericValue(n2))));
opt[Expr] reduceBinary("-", (Expr) `<Numeric n1>`, (Expr) `<Numeric n2>`) = 
  some(mkNumericExpr(subFP(numericValue(n1), numericValue(n2))));
opt[Expr] reduceBinary("*", (Expr) `<Numeric n1>`, (Expr) `<Numeric n2>`) = 
  some(mkNumericExpr(mulFP(numericValue(n1), numericValue(n2))));
opt[Expr] reduceBinary("/", (Expr) `<Numeric n1>`, (Expr) `<Numeric n2>`) = 
  some(mkNumericExpr(divFP(numericValue(n1), numericValue(n2))));

default opt[Expr] reduceBinary(str s, Expr e1, Expr e2) = none();

opt[Expr] reduceUnary("object?", (Expr)`<Literal l>`) = some((Expr) `false`);
opt[Expr] reduceUnary("object?", (Expr)`<Func f>`) = some((Expr) `false`);

opt[Expr] reduceUnary("prim-\>num", (Expr) `<Numeric n>`) = some((Expr) `<Numeric n>`);

opt[Expr] reduceUnary("typeof", (Expr) `<Numeric n>`) = some((Expr) `"number"`);
opt[Expr] reduceUnary("typeof", (Expr) `<Func n>`) = some((Expr) `"function"`);
opt[Expr] reduceUnary("typeof", (Expr) `<String s>`) = some((Expr) `"string"`);
opt[Expr] reduceUnary("typeof", (Expr) `<Bool b>`) = some((Expr) `"boolean"`);
opt[Expr] reduceUnary("typeof", (Expr) `undefined`) = some((Expr) `"undefined"`);
opt[Expr] reduceUnary("typeof", (Expr) `null`) = some((Expr) `"null"`);

default opt[Expr] reduceUnary(str s, Expr e) = none();

Expr reduceAll(subst s, Expr e) = 
  bottom-up visit (e) { 
    case Expr e1: {
      switch(reduce(s, e1)) {
        case some(e2): insert reduceAll(s, e2);
      }
    }
  };
