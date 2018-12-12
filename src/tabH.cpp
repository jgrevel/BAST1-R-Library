#include <Rcpp.h>
#include <string>
#include <fstream>
#include <cmath>
//#include <sstream>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar
double quick_pow10(int n)       // a means to quickly convert 10^int up to 10^15
{
    static long long int pow10[18] = {
        1, 10, 100, 1000, 10000,
        100000, 1000000, 10000000, 100000000, 1000000000,
        10000000000, 100000000000, 1000000000000,
        10000000000000, 100000000000000, 1000000000000000,
        10000000000000000, 100000000000000000
    };

    return double(pow10[n]);          // returns as a double since R cannot handle long long integers
}
double numread(const std::string& text,size_t dp2,size_t edp1,size_t edp2,size_t lpow)// function for converting scientific format numbers in text to double
{
  double numd;                        // variable for return
  if(text.substr(dp2,edp2+4).find("Na")!=std::string::npos)
  {
    numd=nan("");
  }
  else
  {
    const char *t = text.c_str();       // t is a pointer in a position in the char array
                                        // e.g. t  =   0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16
                                        //      *t = ' ','-','1','.','1','2','3','4','5','6','7','8','9','E','+','1','2'
    unsigned long long int num=0;
    int ps=1,p;                         // variables:  power sign (equal 1 or -1), power

    t+= dp2;                            // shift t pointer to position of sign at beginning of number
    bool minus = *t=='-';               // check if negative number
    t+= edp2;                           // shift t pointer to position of E
    for(size_t i=0;i<edp1;i++)             // for loop runs for number of decimal places
    {
      t--;                                  // t pointer is moved back one number in each iteration
      num= num + (*t-'0')*quick_pow10(i);   // number at t is multiplied by increasing multiples of ten and added to total
    }
    t-=2;                               // t pointer is moved back 2 positions, skipping past the decimal place
    num=num+(*t-'0')*quick_pow10(edp1); // first number in *t is multiplied and added to total
                                        // num is now a rendering of the number without the decimal place and without E+##
    t+= edp2;                           // t pointer is shifed the sign occuring after E
    if(*t=='-'){ps=-1;}                 // if the sign is negative ps is switched to -1
    t+=lpow;                                // pointer is moved forward one position, now it is on the first digit of the power number, this is assumed to be a two digit number
    p=0;
    int m=1;
    for(size_t s=0;s<lpow;s++)
    {                             // last digit is multiplied by one, second by 10, and so forth all summed together
      p+= m*(*t-'0');
      t--;
      m=m*10;
    }
    p=p*ps-edp1;                        // power is multipled by power sign and the number of digits is minused from it
    if(abs(p)<18)                       // if power is not too high it can be quicker to use a tabled value than calculate it
    {
      if(p<0)
      {
        numd=num/quick_pow10(abs(p));
      }
      else
      {
        numd=num*quick_pow10(abs(p));
      }
    }
    else
    {
      numd=num*pow(10,p);
    }
    if(minus){numd=-1.0*numd;}          // numb is set to negative if negative
  }
  return numd;
}
// [[Rcpp::export]]
std::vector<std::string> tabH(std::string x)  // this function extracts the header names for the tab file
{
  std::vector<std::string> H;                 // string vector for return
  std::ifstream f;                            // file input stream
  f.open(x.c_str());                          // open file with filepath x (argument supplied to function)
  std::string line,sub;                       // string variables line and sub
  getline(f,line);                            // read in first line (assumed to be unused table file, this might need checking but cirrently isn't)
  getline(f,line);                            // read in second line (first line discarded) this is assumed to be column names
  size_t l=line.length(),p;                      // line length l is declared to be an integer, p will be used for positions in strings (these should really be unsigned but I haven't changed them)
  while(l>0)                                  // while the line has some length, iterate
  {
    p=line.find(" ");                         // p is position of first space in line
    if(p!=0)                                  // if the first character in the line is not a space
    {                                         // then:
      if(p==std::string::npos)                        // if there are no spaces in the string
      {                                               // then:
        H.push_back(line);                                    // write line into string vector
        line.clear();                                         // clear the line, the line now has length zero and the loop should finish
      }
      else                                            // if there is a space in the line (not first position)
      {                                               // then:
        sub=line.substr(0,p);                                 // sub-string of line; from begining upto space is added to string vector
        H.push_back(sub);
      }
    }
    if(p!=std::string::npos)                  // if there is a space in the line
    {                                         // then:
      line.erase(0,p+1);                              // line upto and including first space are removed from line
    }
    l=line.length();                          // length of line is checked again
  }
  f.close();                                  // file connection is closed and string vector returned
  return H;
}
//// [[Rcpp::export]]
//std::vector<double> tabB(std::string x) // Reads the data from the tab file, assumes the data is all numeric in -#.##...#E+## format and all columns have same string width
//{
//  std::vector<double> B;                // numeric vector for return
//  std::ifstream f;                      //file input stream
//  f.open(x.c_str());                    // open file with filepath
//  std::string line,sub;
//  getline(f,line);
//  if(line.length()<16)                    // if line is short (table line), then skip two lines (skip table and skip column headers)
//  {
//    getline(f,line);
//    getline(f,line);
//  }
//  double N;
//  std::vector<size_t> dp2,edp2,edp1;            // positions in string
//
// // Rcout<<"decimal place is "<<dp+1<<std::endl;
// // Rcout<<"E place is "<<E+1<<std::endl;
//  size_t s,E=0,dp=0,l=0;          //
//  while(true)                            // while
//  {
//    E=line.find("E");                     // find the next E
//    if(E==std::string::npos)
//    {
//      break;                              // if no E found exit loop
//    }
//    dp=line.find(".");                   // find .
//    N=numread(line,dp-2,E-dp-1,E-dp+2);   // read in number from line
//    B.push_back(N);                       // add to vector
//    dp2.push_back(l+dp-2);                // add to vectors for column position changes
//    edp1.push_back(E-dp-1);
//    edp2.push_back(E-dp+2);
//    l=l+E+1;                              // keep track of deleted length
//    line.erase(0,E+1);                    // delete the start of the line upto and including the first E
//  }
//
//  s=B.size();                           // s is the number of columns in the file
//                           // length is the number of chars belonging to each column
//  while(f.good())                       // while runs until end of file reached
//  {
//    getline(f,line);                        // read in new line
//    if(line.length()<16)                    // if line is short (table line), then skip two lines (skip table and skip column headers)
//    {
//      getline(f,line);
//      getline(f,line);
//    }
//    if(line.length()>16)                // if line is longer, probably shouldn't need to be asked
//    {
//      for(int i=0;i<s;i++)              // for number of cols
//      {
////        if(B.size()<=s+2)                                   // commented section used in debugging only
////        {
////          Rcout<<"line is: "<<line<<std::endl;
////          Rcout<<"dp2  is: "<<dp2[i]<<std::endl;
////          Rcout<<"edp1 is: "<<edp1[i]<<std::endl;
////          Rcout<<"edp2 is: "<<edp2[i]<<std::endl;
////        }
//        N=numread(line,dp2[i],edp1[i],edp2[i]); // read numbers from positions in line
//        B.push_back(N);                         // add numbers to the reurn vector
//      }
//    }
//  }
//  f.close();
//  return B;
//}

// [[Rcpp::export]]
SEXP tabB(std::string x)
{
  std::vector<std::vector<double> > B;                // numeric vector for return
  std::ifstream f;                      //file input stream
  f.open(x.c_str());                    // open file with filepath
  size_t fs,vl;
  f.seekg (0, f.end);
  fs = f.tellg();
  f.seekg (0, f.beg);

  std::string line,sub;
  getline(f,line);
  if(line.length()<16)                    // if line is short (table line), then skip two lines (skip table and skip column headers)
  {
    getline(f,line);
    getline(f,line);
  }
  vl = fs/line.length();
  double N;
  std::vector<size_t> dp2,edp2,edp1,npow;            // positions in string


  size_t s,E=0,Enan,dp=0,l=0,sp=0;          //
  while(true)                            // while
  {
    std::string oline = line;
    bool fnan=false;
    Enan=line.find(" NaN");
    E=line.find("E");                     // find the next E
    if(Enan<E)
    {
      E=Enan;
      fnan=true;
    }
    if(E==std::string::npos)
    {
      break;                              // if no E found exit loop
    }
    if(fnan)
    {
      N=nan("");
      if(dp2.size()==0)
      {
        dp=3;
      }
      Rcout<<"First item in column "<<dp2.size()+1<<" is NaN;"<<std::endl<<"please check other numbers in column are rendered correctly."<<std::endl;
    }
    else
    {
      dp=line.find(".");                   // find .

    }
    //Rcout<<"decimal place is "<<dp+1<<std::endl;
    //Rcout<<"E place is "<<E+1<<std::endl;
    std::vector<double> empty_vec;
    //empty_vec.reserve(vl);
    B.push_back(empty_vec);
   // B[B.size()-1].reserve(vl);

    dp2.push_back(l+dp-2);                // add to vectors for column position changes   //starts of strings
    edp1.push_back(E-dp-1);                                                               // distance from '.' to 'E' -1, this is the number of places to move back from 'E' to get to the first decimal
    edp2.push_back(E-dp+2);                                                               // distance from '.' to 'E' +2, This is distance from start to 'E' (assuming one digit before decimal)
    l=l+E+1;                              // keep track of deleted length
    line.erase(0,E+1);                    // delete the start of the line upto and including the first E
    sp=line.find(" ");                    // distance to end of number (space after number).
    if(sp==std::string::npos)
    {
      sp=line.length();
    }
    npow.push_back(sp-1);                 // number of power digits
    N=numread(oline,dp-2,E-dp-1,E-dp+2,sp-1);   // read in number from line
    B[B.size()-1].push_back(N);                       // add to vector
  }
  //Rcout<<"first line finished"<<std::endl;

  s=B.size();                           // s is the number of columns in the file
                           // length is the number of chars belonging to each column
  for(size_t i=0;i<s;i++)
  {
    B[i].reserve(vl);
  }
  while(f.good())                       // while runs until end of file reached
  {
    getline(f,line);                        // read in new line
    if(line.length()<16)                    // if line is short (table line), then skip two lines (skip table and skip column headers)
    {
      getline(f,line);
      getline(f,line);
    }
    if(line.length()>16)                // if line is longer, probably shouldn't need to be asked
    {
      for(size_t i=0;i<s;i++)              // for number of cols
      {
//        if(B.size()<=s+2)                                   // commented section used in debugging only
//        {
//          Rcout<<"line is: "<<line<<std::endl;
//          Rcout<<"dp2  is: "<<dp2[i]<<std::endl;
//          Rcout<<"edp1 is: "<<edp1[i]<<std::endl;
//          Rcout<<"edp2 is: "<<edp2[i]<<std::endl;
//        }
        // if(B[i].size()<7)
        // {
        //   Rcout<<line[dp2[i]];                      // should be space or minus sign
        //   Rcout<<line[dp2[i]+edp2[i]-edp1[i]-1];    // should be decimal place '.'
        //   Rcout<<line[dp2[i]+edp2[i]]<<std::endl;   // should be 'E'
        // }

        if( (line[dp2[i]+edp2[i]] != 'E') | (line[dp2[i]+edp2[i]-edp1[i]-1] != '.') )
        {
          IntegerVector err;
          err.push_back(B[i].size()+1);
          err.push_back(i+1);
          return err;
        }
        N=numread(line,dp2[i],edp1[i],edp2[i],npow[i]); // read numbers from positions in line
        B[i].push_back(N);                         // add numbers to the reurn vector
      }
    }
  }
  f.close();
  return wrap(B);
}

//// [[Rcpp::export]]
//std::vector<std::string> tabBC(std::string x)
//{
//  std::vector<std::string> B;
//  std::ifstream f;
//  f.open(x.c_str());
//  std::string line,sub;
//  getline(f,line);
//  getline(f,line);
//  getline(f,line);
//  int l=line.length(),p,len,s;
//  len=l;
//  while(l>0)
//  {
//    p=line.find(" ");
//    if(p!=0)
//    {
//      if(p==std::string::npos)
//      {
//        B.push_back(line);
//        line.clear();
//      }
//      else
//      {
//        sub=line.substr(0,p);
//        B.push_back(sub);
//      }
//    }
//    if(p!=std::string::npos)
//    {
//      line.erase(0,p+1);
//    }
//    l=line.length();
//  }
//  s=B.size();
//  len=len/s;
//  while(f.good())
//  {
//    getline(f,line);
//    if(line.length()<16)
//    {
//      getline(f,line);
//      getline(f,line);
//    }
//    if(line.length()>16)
//    {
//      for(int i=0;i<s;i++)
//      {
//        B.push_back(line.substr(len*i,len));
//      }
//    }
//  }
//  f.close();
//  return B;
//}
