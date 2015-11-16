module lambdajs::Env

import lambdajs::Syntax;
import ParseTree;
import IO;

public Env parseEnv(str txt) = parse(#start[Env], txt).top;

public Env parseEnvFile(loc fname) = parseEnv(readFile(fname));
