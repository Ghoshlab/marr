
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(.seqen)]]
NumericVector seqen(int n){
  NumericVector y(n);
  y=seq(1,n);
  return y/n;
}
// [[Rcpp::export(.reptest2)]]
NumericVector reptest2(int x,int y) {
  NumericVector myvector(y);
  for (int i = 0; i < y; ++i) {
    myvector[i] = x;
  }
  return myvector;
}

// [[Rcpp::export(.cumsumSS)]]
NumericVector cumsumSS(NumericVector x){
  // initialize an accumulator variable
  double acc = 0;

  // initialize the result vector
  NumericVector res(x.size());

  for(int i = 0; i < x.size(); i++){
    acc += x[i];
    res[i] = acc;
  }
  return res;
}

// [[Rcpp::export(.subset_range1)]]
NumericVector subset_range1(NumericVector x,
                            int start, int end) {

  // Use the Range function to create a positional index sequence
  return x[Rcpp::Range(start, end)];
}

// [[Rcpp::export(.ssapplySSS)]]
NumericVector sapplySSS(NumericVector xx){
  int sizexx=xx.size();
  NumericVector myW(xx.size());
  for(int i = 0; i < sizexx; i++){
    myW[i]=sum(xx==i+1);
  }
  return myW;
}

// .getSS: an internal Rcpp function to produce a numeric vector of
// sums of squared differences between observed and actual survival
// functions for k-hat=0,...,n-1
// INPUTS: maxRank = a vector of maximum rank statistics
// OUTPUTS: mySS = a vector of SS(i/k) values as described in
// Philtron et al., 2018
// [[Rcpp::export(.getSS)]]
NumericVector getSS(NumericVector maxRank) {
  int n=maxRank.size();
  NumericVector x(n);
  x= seqen(n);
  NumericVector mySS(n);
  NumericVector myWW(n);
  NumericVector xx(n);
  xx= reptest2(1,n);

  myWW= sapplySSS(maxRank);
  NumericVector vn(n);
  vn=reptest2(n,n);
  NumericVector survFunction(n);
  survFunction = (vn-cumsumSS(myWW))/n;

  int i=0;
  long double pi1=0;
  long double pi0=0;
  for(int k = 0; k < n; k++) {
    i=k+1;
    double n1=0;
    n1=n;
    pi1=k/n1;
    pi0=1-pi1;
    NumericVector pi1rep(n);
    pi1rep= reptest2(pi1,n);
    NumericVector tempW(n-k);
    tempW=subset_range1(survFunction,k,n-1);
    NumericVector pisum(n);
    //double pisum=0;
    NumericVector pisum1(n);
    //pisum1=(x-pi1)*(x-pi1)/(pi0*pi0);
    pisum1=(x-pi1)*(x-pi1)/(pi0*pi0);
    pisum=pi0*(1-pisum1);
    NumericVector Sn(n-k);
    //pisum=pisum1;
    //pisum=sum(tempW);
    Sn =subset_range1(pisum,k,n-1);
    NumericVector tempDiff(n-k);
    tempDiff = tempW-Sn;
    NumericVector sqDiff(n-k);
    sqDiff = (tempDiff*tempDiff)/n;

    mySS[k] = (sum(sqDiff))/pi0;

  }
  return mySS;
}

// [[Rcpp::export(.seqenkhat)]]
NumericVector seqenkhat(int n, int k){
  NumericVector y(n-k);
  y=seq((k+1),n);
  return y;
}

// [[Rcpp::export(.sapplykhat)]]
NumericVector sapplykhat(int khat, NumericVector maxRank){
  int n=maxRank.size();
  NumericVector Nhat(n-khat);
  Nhat = seqenkhat(n,khat);
  int sizexx=Nhat.size();
  NumericVector myWW(sizexx);
  for(int i = 0; i < sizexx; i++){
    myWW[i]=sum(maxRank<= Nhat[i]);
  }
  return myWW;
}

// [[Rcpp::export(.RNhatcpp)]]
IntegerVector RNhatcpp(int khat,NumericVector maxRank)
{
  int n=maxRank.size();
  IntegerVector RNhat(n-khat);
  RNhat = sapplykhat(khat,maxRank);
  return RNhat;
}
