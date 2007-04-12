#include <stdio.h>

class constr
{
public:
  constr ();
};

constr::constr()
{
  printf ("Constructor called\n");
}

constr a_constr;

int
main (void)
{
  printf ("Hello cpp\n");
  return 0;
}
