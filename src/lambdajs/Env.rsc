module lambdajs::Env

import lambdajs::Syntax;
import ParseTree;
import IO;

public start[Env] parseEnv(str txt) = parse(#start[Env], txt);

public start[Env] parseEnvFile(loc fname) = parse(#start[Env], fname);
