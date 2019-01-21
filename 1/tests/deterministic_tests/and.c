a = true;
b = false;
assert(a && a);
assert(!(a && b));
assert(!(b && a));
assert(!(b && b));