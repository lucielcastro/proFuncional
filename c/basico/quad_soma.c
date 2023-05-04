#include<stdio.h>

int quad( int x);
int qsoma(int n, int x);

int main(){
   printf("%d",quad(qsoma(4, 4)));
   return 0;
}

int quad(int x){
   return x * x;
}

int qsoma(int n, int x){
   return n + x;
}