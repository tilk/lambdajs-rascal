module lambdajs::FreshNames

import ParseTree;
import IO;
import Option;
import lambdajs::Syntax;

anno int Tree@seq; // TODO: somehow does not work on Id

alias Scope = map[str, int];

private int seq = 0;

private int newSeq() {
  seq = seq + 1;
  return seq; 
}

tuple[Scope, start[Env]] freshen(Scope scope, start[Env] tree) {
  tree = top-down visit (tree) {
    case ed: (EnvDef) `let [<Id i>] = <Expr e>`: {
      s = newSeq();
      i = i[@seq = s];
      e = freshen(scope, e);
      scope = scope + ("<i>": s);
      insert ed[i=i][e=e];
    }
    case ed: (EnvDef) `rec [<Id i>] = <Expr e>`: {
      s = newSeq();
      i = i[@seq = s];
      scope = scope + ("<i>": s);
      e = freshen(scope, e);
      insert ed[i=i][e=e];
    }
    case ed: (EnvDef) `{ <Expr e> }`: {
      e = freshen(scope, e);
      insert ed[e=e];
    }
  };
  return <scope, tree>;
}

start[Prog] freshen(Scope scope, start[Prog] tree) =
  visit (tree) {
    case p: (Prog) `<Expr e>`: {
      e = freshen(scope, e);
      insert p[e=e];
    }
  };

Expr freshen(Scope scope, Expr tree) =
  top-down-break visit (tree) {
    case e: (Expr)`<Func f>`: {
      Scope newscope = ();
      is = visit(f.is) { 
        case Id i: { 
          s = newSeq();
          newscope["<i>"] = s;
          insert i[@seq=s]; 
        } 
      };
      fe = freshen(scope + newscope, f.e);
      insert e[v=e.v[f=f[e=fe][is=is]]];
    }
    case e: (Expr) `let (<Id i> = <Expr e1>) <Expr e2>`: {
      s = newSeq();
      i = i[@seq = s];
      e1 = freshen(scope, e1);
      e2 = freshen(scope + ("<i>": s), e2);
      insert e[i=i][e1=e1][e2=e2];
    }
    case e: (Expr) `rec (<Id i> = <Expr e1>) <Expr e2>`: { 
      s = newSeq();
      i = i[@seq = s];
      newscope = scope + ("<i>": s);
      e1 = freshen(newscope, e1);
      e2 = freshen(newscope, e2);
      insert e[i=i][e1=e1][e2=e2];
    }
    case e: (Expr) `<Id i>`: {
      if ("<i>" in scope) 
        i = i[@seq = scope["<i>"]];
      i@seq; // TODO better test for presence of @seq
      insert e[v=e.v[i=i]];
    }
  };

private map[int, Expr] mkSubstMap(map[Id, Expr] s) = (i@seq: freshen((), s[i]) | i <- s);

alias subst = opt[Expr](Id);

subst mkSubst(map[Id, Expr] s) {
  map[int, Expr] ss = mkSubstMap(s);
  opt[Expr] f(Id i) { if(i@seq in ss) return some(ss[i@seq]); else return none(); };
  return f;
}

Expr substs(Expr e, subst s) {
  return visit(freshen((), e)) {
    case (Expr) `<Id i>`: { 
      opt[Expr] r = s(i);
      if (isNone(r)) fail;
      else switch (r) { case some(rr): insert rr; };
    }
  }
}

Expr substsAll(Expr e, subst s) {
  return visit(freshen((), e)) {
    case (Expr) `<Id i>`: {
      opt[Expr] r = s(i);
      if (isNone(r)) fail;
      else switch (r) { case some(rr): insert substsAll(rr, s); };
    }
  }
}

Expr subst(Expr e, Id i, Expr e2) = substs(e, mkSubst((i: e2)));
