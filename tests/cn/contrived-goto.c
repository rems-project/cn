/* A contrived example with labels and gotos, for making sure Fulminate's C control-flow machinery works */
int f(int flag) {
    int a = 'h';
    if (flag) {
      goto L1;
    }
    int b = 'w';
  
    {
      int x = 1;
  
      {
        int z = 42;
        /*
        ...
        */
        return 0;
      }
  
    L1: {
      int y = 2;
      x = 5;
      if (!flag) {
        goto L2;
      }
      int a = 5;
  
      /*
      ...
      */
    }
  
      /*
      ...
      */
    }
  
  L2:
    a = 's';
    return 0;
  }
  
  int main(void) {
    f(1);
    return 0;
  }
  