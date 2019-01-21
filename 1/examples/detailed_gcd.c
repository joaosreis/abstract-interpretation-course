a = -26134272;
b = 26396928;

print(a, b);
if (a < 0) a = -a;
print(a, b);
if (b < 0) b = -b;
print(a, b);
while (b > 0) {  // try changing this to b >= 0
  print(a, b);
  tmp = b;
  b = a % b;
  a = tmp;
 }
assert(a == 131328);
print (a); // this should print 131328

