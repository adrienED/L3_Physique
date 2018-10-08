#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("bet.dat");

int main (void){
int n=1, i=0,j;
double x=0.0, p=1.0 , xx[n], g;
for(int j=0; j<=n;j++)
xx[j]=(M_PI/n)*j;

x=xx[j];
while(x<=M_PI){
fich<<"x = "<<xx[j]<<"     "<<g<<endl;
x=x+0.01;
}
fich.close();
return 0;


}

