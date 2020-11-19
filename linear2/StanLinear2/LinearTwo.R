# file name : LinearTwo.R
# Stan under simple multi modal parameter distribution space
# Please refer to LinearTwo.html 
# setwd("C:/Users/lanhsieh/Documents/fitdist/linear2/StanLinear2")

get_CXX <- function(CXX14 = TRUE) {
  if (.Platform$OS.type != "windows")
    return (system2(file.path(R.home(component = "bin"), "R"),
                    args = paste("CMD config", ifelse(CXX14, "CXX14", "CXX11")),
                    stdout = TRUE, stderr = FALSE))
  
  ls_path <- Sys.which("ls")
  if (ls_path == "")
    return(NULL)
  
  install_path <- dirname(dirname(ls_path))
  file.path(install_path,
            paste0('mingw_', Sys.getenv('WIN')), 'bin', 'g++')
}

{# Setup : load (or install + load) required packages
  ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  }
  
  pckToLoad = c("rstan")
  
  reloadpck = function(reload = TRUE, pck = pckToLoad){
    if(reload){
      print(ipak(pck))
      rstan_options(auto_write = TRUE)
      options(mc.cores = parallel::detectCores() - 1)
    }
  }
  reloadpck()
}

# Data
{
  x = 1:10
  y1 = c( -1.006879, -2.001636, -2.993538, -3.994545, -5.008485, 
          -6.006281, -6.977793, -8.004303, -8.999835, -9.992217)
  
  y2 = c( +1.006879, +2.001636, +2.993538, +3.994545, +5.008485, 
          +6.006281, +6.977793, +8.004303, +8.999835, +9.992217)
  
  if(F) {# Visualisation
    plot(x,y1,ylim = c(-10,10), col = "red")
    par(new = T)
    plot(x,y2,ylim = c(-10,10), col = "black")
  }
}

# Compile Stan model
{
  tmp.stanfile.name = "LinearTwo"
  
  recompil = TRUE   # Switch to TRUE to compile stan model ; Switch to FALSE to load previous stan model
  saveNewModel = TRUE
  
  if(recompil){
    model_LinearTwo = stan_model(stanc_ret = stanc(paste0(tmp.stanfile.name,".stan")))
    
    if(saveNewModel){
      saveRDS(model_LinearTwo, file = paste0(tmp.stanfile.name,".stanprog"))
    }
  }else{
    model_LinearTwo = readRDS(paste0(tmp.stanfile.name,".stanprog"))
  }
}

# Stan input
{
  stan_input.LinearTwo = list(
    "nb"   = length(x),
    "x"    = x  ,
    "y1"   = y1 ,
    "y2"   = y2
  )
}

# Fit
## Load privious saved fits
{
  print("Some fits are available in the working directory. You can load them")
  print(list.files(pattern = ".stanFit"))
  
  loadPrevious = F
  tmp.fit_ID = 1 # the first previously-listed *.stanfit will be selected

  if(loadPrevious) {
    fit_LinearTwo = readRDS(list.files(pattern = ".stanFit")[tmp.fit_ID])
    # fit_LinearTwo = readRDS("LinearTwo_Chain_20_iter_10000.stanFit")
  }
}

## New stan fit
{
  refit = T       # Switch to TRUE to fit the data ; Switch to FALSE to load previous fits
  SaveNewFit = T
  
  nb.chains = 3
  nb.iter = 2000000
  my.seed = 2020
  
  if(refit){
    # General linear DBN
    fit_LinearTwo = sampling(
      model_LinearTwo,
      data = stan_input.LinearTwo,
      chains = nb.chains,
      iter = nb.iter,
      warmup = floor(nb.iter/2),
      seed = my.seed
    )
    
    if(SaveNewFit){
      saveRDS(fit_LinearTwo,
              file = gsub( "\\+", "", 
                          paste0(tmp.stanfile.name,"_Chain_",nb.chains,"_iter_",nb.iter,".stanFit")))
    }

  }
}

# summary
fit_LinearTwo

tmp.fit = fit_LinearTwo

check_hmc_diagnostics(tmp.fit)

{
  # Trace plot for the first 250 iteration in warmup phase
  stan_trace(tmp.fit, pars=c("beta",
                             "sigma1",
                             "sigma2",
                             "lp__"),
             inc_warmup = T,
             window = c(0,250),
             alpha = 0.2)
  
  # Trace plot without warmup
  stan_trace(tmp.fit, pars=c("beta",
                             "sigma1",
                             "sigma2",
                             "lp__"),alpha = 0.2
  )
  
  # Posterior densities
  stan_dens(tmp.fit, pars=c("beta",
                            "sigma1",
                            "sigma2",
                            "lp__"))
}

# extract MCMCs

tmp.fit.extr = 
  rstan::extract(tmp.fit, pars=c("beta",
                                 "sigma1",
                                 "sigma2",
                                 "lp__"), 
                 permuted = FALSE,
                 inc_warmup = TRUE)

head(tmp.fit.extr[ , , parameters = "beta"] , 10)
# tmp.fit.extr[ , , parameters = "sigma1"]
# tmp.fit.extr[ , , parameters = "sigma2"]
# tmp.fit.extr[ , , parameters = "lp__"]
