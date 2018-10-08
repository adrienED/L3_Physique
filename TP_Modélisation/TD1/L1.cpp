#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;


	
int main (void){
int n=3, i=0,j;
double x=1.1, p=1.0 , xx[n];
xx[0]=0.0;
xx[1]=0.5;
xx[2]=1.0;
xx[3]=2.0;

for (j=0; j<=n; j++) {
if (j!=i) p=p*(x-xx[j])/(xx[i]-xx[j]);
}
cout<<" x="<<x<<" i="<<i<<" p="<<p<<endl;

return 0;
}

