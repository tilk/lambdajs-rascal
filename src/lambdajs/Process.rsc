module lambdajs::Process

import IO;
import lambdajs::Syntax;
import lambdajs::Env;
import lambdajs::FreshNames;
import lambdajs::Reduce;
import lambdajs::Normalize;

start[Prog] process(loc envloc, start[Prog] prog) {
  start[Env] env = parseEnv(envloc);
  env = normalize(env);
  <scope, env> = freshen((), env);
  prog = freshen(scope, prog);
  lf = letFuncs(env);
  lfs = mkSubst(lf);
//  prog = visit(prog) { case Expr e => substsAll(e, lfs) };
  prog = top-down-break visit(prog) { case Expr e => reduceAll(lfs, e) };
  return prog;
}
