#include <Rcpp.h>
using namespace Rcpp;
using namespace std;


// This function is not intended for direct use by the end user, but rather to be embedded inside an R function (strat_stats)
// This function thus, should not be included in the NAMESPACE file.
// [[Rcpp::export]]
DataFrame cppStats(IntegerVector id,NumericVector da)    // id and da should be sorted prior to passing to function
{
//  the data entering this function should already be sorted, primarily be id, and secondarily by da. da should not contain any NAs/NaNs

  vector<double> sum,min,max,mean,median,altsd;
  vector<long double> dinitSum;//sqsum
  vector<int> lengths,starts;
  int count=-1,prev=NA_INTEGER;
  size_t s=id.size();
  for(size_t i=0;i<s;i++)       // loop through all data
  {
    if(prev!=id[i])             // this is true when a new id is found
    {
      starts.push_back(i);      // length of starts vector is increased and set
      min.push_back(da[i]);     // length of min is increased and set
      max.push_back(da[i]);     // length of max is increased and initialised (but fil be updated)
      lengths.push_back(0);     // length of lengths is increased and initialised to 0
      sum.push_back(0);         // length of sum is increased and initialised to 0

      dinitSum.push_back(0);    // This vector is for collecting the sum of the squared differences between each data item and the first
                                // value observed. This will be used to calculate the standard deviation using a transform applied at
                                // the end, once the true mean is known. This method was prefered over collecting the sum of the squared
                                // values, since it reduces the introduction of rounding errors (otherwise caused by subracting very
                                // large numbers to determine small numbers).

      count++;                  // a track of the current index in the vectors
    }
    prev=id[i];                 // prev is used to check when a new id has occured
    lengths[count]++;           // lengths, sum, max and dinitSum are all updated for each data item
    sum[count]+=da[i];
    max[count] =da[i];
    long double blah=da[i]-min[count];
    dinitSum[count] += blah*blah;
  }
  for(int i=0;i<count+1;i++)    // for loop runs to the length of the vectors previously built (i.e. one itteration per unique id)
  {
    mean.push_back(sum[i]/double(lengths[i]));        // mean da for each id
    int l = lengths[i]/2;                             // half of length, rounded down
    median.push_back(da[starts[i]+l]*0.5+da[starts[i]+lengths[i]-1-l]*0.5); // median da for each id

    long double delM=mean[i]-min[i],lm=lengths[i]-1;  // delM is the difference between the true mean and the first observed
                                                      // value for the given id.
    long double VAR,ldm=min[i];
    VAR = lengths[i]*delM*delM/lm  + dinitSum[i]/lm + 2*delM*(lengths[i]*ldm/lm - sum[i]/lm) ;  // Variance is calculated
    altsd.push_back(sqrt(VAR) );
  }
 return DataFrame::create(_["n"]=lengths,_["min"]=min,_["max"]=max,_["sum"]=sum,_["mean"]=mean,
                          _["median"]=median,_["SD"]=altsd,_["starts"]=starts);
}

