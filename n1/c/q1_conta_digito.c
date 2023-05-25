#include <stdio.h>

int conta_digito(int n);

int main(){
   printf("%d", conta_digito(-2993));
   return 0;
}

int conta_digito(int n){
   if(n>-10 && n<10)
      return 1;
   n/=10;
   return 1+conta_digito(n);
}