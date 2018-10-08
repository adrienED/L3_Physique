#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("exp.dat");


	
int main (void){
int n=10, i=0, j;
double x=-1.0, p=1.0 , xx[n];
for(int j=0; j<=n;j++) 
xx[j]=(-1.+2*j/n);
if (j!=i) p=p*((x-xx[j])/(xx[i]-xx[j]));
x=xx[j];
while(x<1){

fich<<x<<"       "<<exp(x)*p<<endl;
x=x+0.01;

}

fich.close();
return 0;
}


