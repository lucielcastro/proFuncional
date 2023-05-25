#include<stdio.h>

int suc( int x);

int main(){
   printf("%d",suc(4));
   return 0;
}

int suc(int x){
   return x + 1;
}