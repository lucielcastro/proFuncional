#include<stdio.h>

int ant( int x);

int main(){
   printf("%d",ant(4));
   return 0;
}

int ant(int x){
   return x - 1;
}