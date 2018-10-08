#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
using namespace std;
ofstream fich("data.f");

int main (void){

int k=0,i,j,M=50,N=50;
double ER=1,ERREUR,oldvalue,h=0.02;

double f[M][N],D[M][N];

for(i=1;i<N-1;i++){
for(j=1;j<M-1;j++){
f[i][j]=0.0;D[i][j]=0.0;
}}
D[25][25]=50;

while (ER>0.001){
	ERREUR=0;
	for(i=1;i<N-1;i++){
	for(j=1;j<M-1;j++){
	oldvalue=f[i][j];
	f[i][j]=0.25*(f[i+1][j]+f[i-1][j]+f[i][j+1]+f[i][j-1])-0.25*h*h*D[i][j];
	ERREUR=ERREUR+abs(f[i][j]-oldvalue);
}}
k=k+1;
ER=ERREUR;
cout<<k<<" "<<ER<<endl;

}

for(i=1;i<N-1;i++){
	for(j=1;j<M-1;j++){
	f[i][j]=0.25*(f[i+1][j]+f[i-1][j]+f[i][j+1]+f[i][j-1])-0.25*h*h*D[i][j];

fich<<i<<" "<<j<<" "<<f[i][j]<<endl;
}
}
fich.close();
	
	 
}


