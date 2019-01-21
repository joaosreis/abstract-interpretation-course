a = 0;
if(true)
    a = a + 1;

if(false)
    a = a + 10;

if(true)
    a = a + 100;
else
    a = a + 200;

if(false)
    a = a + 1000;
else
    a = a + 2000;

assert(a == 1 + 100 + 2000);