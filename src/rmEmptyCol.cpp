#include <Rcpp.h>
using namespace Rcpp;


/*
 * allNA is an overloaded set of functions. It is set up to deal with CharacterVectors, NumericVectors and IntegerVectors.
 * All of these functions return a bool which is false if anything in the Vector is deemed to contain information. And true
 * if the vector contains only NAs or equivalents. These functions are internal and are used by KeepCols, KeepColsCM and KeepColsNM.
 *
 * The CharacterVector version of this function requires an additional argument that is a CharacterVector of strings that are to
 * be considered as NAs.
 */
bool allNA(CharacterVector &cv,CharacterVector &naString)               // Check if all elements of a character vector are empty (or have no data)
{
  int l=cv.length();                                                        // length of vector
  int nasl = naString.length();
  for(int i=0;i<l;i++)
  {
    bool isna;                                                              // a bool for a given element
    isna = cv[i]==NA_STRING || cv[i]=="NA" || cv[i]=="NaN" || cv[i]=="" ;   // set of strings to always check
    for(int j=0;j<nasl;j++)
    {
      isna = isna || cv[i]==naString[j];                                    // addition optional checks
    }
    if(!isna)
    {
      return false;                                                         // if any element is not NA then the vector will not be all NA
    }                                                                       // so result can be returned without wasting effort on further checks

  }
  return true;
}

bool allNA(NumericVector &nv)                            // Check if all elements of a numeric vector are NaN (NA in R becomes NaN when
{                                                       // coerced to a c++ double)
  int l=nv.length();
  for(int i=0;i<l;i++)
  {
    bool notna;
    notna = nv[i] == nv[i];                               // NaN is not equal to NaN, other numbers are always identical to themselves

    if(notna)
    {
      return false;
    }

  }
  return true;
}

bool allNA(IntegerVector &iv)                            // Check if all elements of an integer vector are NA
{
  int l=iv.length();
  for(int i=0;i<l;i++)
  {
    bool notna;
    notna = iv[i] != NA_INTEGER;

    if(notna)
    {
      return false;
    }

  }
  return true;
}



/*
 * KeepCols, KeepColsCM and KeepColsNM are all functions that are exported to R but will be used internally by a wrapper function written in R.
 * These functions take the main data sources (either matrix or data.frame) as first argument.
 * KeepCols is used on DataFrames and takes an arguement giving it the column classes. Additionally it takes the optional naStrings as an arguement.
 * KeepColsCM is used on matrices of mode Character. naStrings argument also must be supplied.
 * KeepColsNM is used on matrices of mode numeric or logical. The matrix is the only supplied arguement.
 * Each of these functions returns a logical vector; with true for columns containing infomation, and false for columns with no information
 * (NAs etc.).
 */

// [[Rcpp::export]]
LogicalVector KeepCols(DataFrame &df,CharacterVector &classes,CharacterVector &naStrings) {  // method to deal with data.frames
  LogicalVector lv;
  int c = df.length();
  for(int i=0;i<c;i++)                                                // Treat one column at a time
  {
    if(classes[i]=="character" || classes[i]=="factor")                 // factors are treated as character
    {
      CharacterVector Col = df[i];
      lv.push_back(!allNA(Col,naStrings));
    }
    else if(classes[i]=="numeric")
    {
      NumericVector Col = df[i];
      lv.push_back(!allNA(Col));
    }
    else if(classes[i]=="integer" || classes[i]=="logical")             // logicals are treated as integers
    {
      IntegerVector Col = df[i];
      lv.push_back(!allNA(Col));
    }
    else                                                              // If a column of some other class is found an empty vector is returned
    {                                                                 // The wrapper function (in R) will detect this and return an error.
      LogicalVector bad;
      return bad;
    }
  }
  return lv;
}

// [[Rcpp::export]]
LogicalVector KeepColsCM(CharacterMatrix &CM,CharacterVector &naStrings) {
  LogicalVector lv;
  int c = CM.ncol();
  for(int i=0;i<c;i++)
  {
    CharacterVector Col = CM.column(i);
    lv.push_back(!allNA(Col,naStrings));
  }
  return lv;
}

// [[Rcpp::export]]
LogicalVector KeepColsNM(NumericMatrix &NM) {
  LogicalVector lv;
  int c = NM.ncol();
  for(int i=0;i<c;i++)
  {
    NumericVector Col =NM.column(i);
    lv.push_back(!allNA(Col));
  }
  return lv;
}




