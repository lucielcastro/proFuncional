#include <stdio.h>

int div( int n, int d);

int main(){
   printf("%d", div(8, 2));
   return 0;
}

int div(int n, int d){
   if (n < d)
      return 0;
   return 1 + div(n-d, d);
}