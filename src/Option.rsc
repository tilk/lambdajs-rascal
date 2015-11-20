module Option

data opt[&T] = some(&T this) | none();

bool isNone(none()) = true;
bool isNone(some(_)) = false;
