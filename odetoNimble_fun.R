odetoNimble <- function(name, sys, pars)
{
  
  Nparam = paste(paste0(rep("par[",length(pars)),seq(0,length(pars)-1,1),rep("]",length(pars))),collapse=",")
  
  nimbleWrap.footer <- paste0('
  
  void nimble_wrap_',name,'(double *initR, int linitR, double *timesR, int ltimesR, double step_size, double start, double *ans, double *parR, int lparR) {
    Rcpp::NumericVector init = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(std::vector<double>(initR, initR+linitR)));
    Rcpp::NumericVector par = Rcpp::as<Rcpp::NumericVector>(Rcpp::wrap(std::vector<double>(parR, parR+lparR)));
    std::vector<double> times = std::vector<double>(timesR, timesR+ltimesR);
    ',name,'_set_params(',Nparam,');
    Rcpp::List ansTMP = ',name,'_at(init, times, step_size, start);
    
  // Rcpp::Rcerr << "',name,' return length : " << ansTMP.size() << std::endl;
    Rcpp::CharacterVector anstmpname = ansTMP.names();
  // for(int i = 0; i<anstmpname.size(); i++)
  //    Rcpp::Rcerr << anstmpname[i] << " " ;
  //  Rcpp::Rcerr <<  std::endl;

    Rcpp::NumericVector numVec = ansTMP["Time"];
  //  Rcpp::Rcerr << "Time size (nstep) : " << numVec.size()  << std::endl;
    
    /* Fill an array
    // fill ans by row Time, X1, X2...
    // For all list element
    for(int list_index = 0; list_index<anstmpname.size(); list_index++)
    {
      numVec = ansTMP[list_index];
      for(int i=0; i<numVec.size(); i++)
        ans[list_index + i*anstmpname.size()] = numVec[i];
    }
    */
    
    // Fill a matrix
    Rcpp::NumericMatrix mat = Rcpp::no_init(numVec.size(), anstmpname.length());
    
    for(unsigned int i = 0; i < anstmpname.size(); i++) {
      Rcpp::NumericVector col_val = ansTMP[i];
      mat(Rcpp::_,i) = col_val;
    }
    // save matrix into ans
    std::copy(mat.begin(), mat.end(), ans);
  }')
  
  nimbleWrap.header =  '#include "nimble_wrap.h"//'
  
  # create cpp code
  thecode = compile_implicit(name, sys, pars,
                        const = TRUE,
                        compile = FALSE,
                        headers = nimbleWrap.header,
                        footers = nimbleWrap.footer
  )

  # create a header file
  sink('nimble_wrap.h')
  cat(paste0('
    #ifndef __NIMBLE_WRAP_CPP__
    #define __NIMBLE_WRAP_CPP__

    void nimble_wrap_',name,'(double *initR, int linitR, double *timesR, int ltimesR, double step_size, double start, double *ans, double *parR, int lparR);
 
    #endif
  '))
  sink()
  
  # write cpp file
  sink(paste0(name,'.cpp'))
  cat(thecode)
  sink()
  
  # compile .o using Rccp sourceCpp
  Rcpp::sourceCpp(file=paste0(name,".cpp"),showOutput = FALSE, rebuild=TRUE, embeddedR=FALSE)
  # get the .o generated
  objectc <- list.files(getOption("rcpp.cache.dir", tempdir()),paste0(name,".o") , recursive=TRUE, full.names=TRUE, include.dirs=TRUE)
  file.copy(objectc[1], paste0(name,".o"), overwrite = TRUE)
  
  nExtCall <<- nimbleExternalCall(prototype = function(initR = double(1),
                                          linitR = integer(),
                                          timesR = double(1),
                                          ltimesR = integer(),
                                          step_size = double(),
                                          start=double(),
                                          ans = double(2),
                                          parR = double(1),
                                          lparR = integer()){},
                     Cfun =  paste0('nimble_wrap_',name),
                     headerFile = file.path(getwd(), 'nimble_wrap.h'),
                     returnType = void(),
                     oFile = file.path(getwd(), paste0(name,'.o')))
  
  nimbleFunctionModel <- nimbleFunction(
    run = function(init = double(1), times = double(1), step_size = double(), par = double(1)) {
      ans <- matrix(nrow=length(times), ncol=length(init)+1)
      nExtCall(init, length(init), timesR=times, ltimesR=length(times), step_size=step_size, start=0.0, ans, par, length(par))
      return(ans)
      returnType(double(2))
    })
  
  return(nimbleFunctionModel)
}
