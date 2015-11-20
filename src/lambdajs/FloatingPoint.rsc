module lambdajs::FloatingPoint

alias float = str;

@javaClass{lambdajs.FloatingPoint}
java float addFP(float v1, float v2);

@javaClass{lambdajs.FloatingPoint}
java float subFP(float v1, float v2);

@javaClass{lambdajs.FloatingPoint}
java float mulFP(float v1, float v2);

@javaClass{lambdajs.FloatingPoint}
java float divFP(float v1, float v2);
