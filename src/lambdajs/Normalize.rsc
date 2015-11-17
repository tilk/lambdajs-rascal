module lambdajs::Normalize

import lambdajs::Syntax;

Expr normalize((Expr) `<Expr e1> + <Expr e2>`) = (Expr)`prim("+", <Expr e1>, <Expr e2>)`;
Expr normalize((Expr) `<Expr e1> - <Expr e2>`) = (Expr)`prim("-", <Expr e1>, <Expr e2>)`;
Expr normalize((Expr) `<Expr e1> * <Expr e2>`) = (Expr)`prim("*", <Expr e1>, <Expr e2>)`;
Expr normalize((Expr) `<Expr e1> / <Expr e2>`) = (Expr)`prim("/", <Expr e1>, <Expr e2>)`;
Expr normalize((Expr) `<Expr e1> == <Expr e2>`) = (Expr)`prim("==", <Expr e1>, <Expr e2>)`;
Expr normalize((Expr) `<Expr e1> != <Expr e2>`) = (Expr)`prim("!", prim("==", <Expr e1>, <Expr e2>))`;
Expr normalize((Expr) `<Expr e1> === <Expr e2>`) = (Expr)`prim("===", <Expr e1>, <Expr e2>)`;
Expr normalize((Expr) `<Expr e1> !== <Expr e2>`) = (Expr)`prim("!", prim("===", <Expr e1>, <Expr e2>))`;
Expr normalize((Expr) `<Expr e1> || <Expr e2>`) = (Expr)`if (<Expr e1>) true else <Expr e2>`;
Expr normalize((Expr) `<Expr e1> && <Expr e2>`) = (Expr)`if (<Expr e1>) <Expr e2> else false`;
Expr normalize((Expr) `!<Expr e>`) = (Expr)`prim("!", <Expr e>)`;
Expr normalize((Expr) `-<Expr e>`) = (Expr)`prim("-", <Expr e>)`;
Expr normalize((Expr) `closure?<Expr e>`) = (Expr)`prim("closure?", <Expr e>)`;  
Expr normalize((Expr) `primitive?<Expr e>`) = (Expr)`prim("primitive?", <Expr e>)`;  
Expr normalize((Expr) `object?<Expr e>`) = (Expr)`prim("object?", <Expr e>)`;  
Expr normalize((Expr) `typeof<Expr e>`) = (Expr)`prim("typeof", <Expr e>)`;
Expr normalize((Expr) `if (<Expr e>) <Expr e1>`) = (Expr)`if (<Expr e>) <Expr e1> else undefined`;  
default Expr normalize(Expr e) = e;

start[Env] normalize(start[Env] env) = visit(env) { case Expr e => normalize(e) };
start[Prog] normalize(start[Prog] env) = visit(env) { case Expr e => normalize(e) };

