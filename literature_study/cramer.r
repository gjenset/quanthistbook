### Cramer's V

### Defining a function to calculate the Cramér V and Phi in R


#print.noquote("Cramér V measure of effect size (Phi for 2 X 2 tables).")
print.noquote("Usage: cv.test(matrix)")

cv.test <- function(x){

if (min(dim(x)) < 2)    
	stop("Invalid input! Matrix must be at least 2 x 2", call.=F)   # don't allow 1 x n matrices
	
	options(warn=-1)          # switch off warnings for the chisq.test
	
# --------------------------------------------------------------------#	
	
        CV <- sqrt(chisq.test(x, correct = FALSE)$statistic / 
		(sum(x) * min(dim(x) - 1 )))                # computing Phi/V

    

        CV = as.numeric(CV)
        
# --------------------------------------------------------------------#

      R2 <- round(CV^2, 6) 
      # find proportion of explained variance as squared phi/V
      # this is broadly similar to taking the squared Pearson correlation coef.
      # Note that this is a strong assumption to make!        
             
# -------------------------------------------------------# 
  # Get the degrees of freedom:
  
      DF <- as.numeric(chisq.test(x, correct = FALSE)$parameter)

# ------------------------------------------------------- #
  
   nd <- min(dim(x))     # min size of matrix
   input.name = deparse(substitute(x))   # variable name of matrix
  

# ---------------------------------------------------- #
# Determine which test has been used:  
  if(max(dim(x))==2)
	#print.noquote("Phi:")
	test <- "Phi"
	
	else
	#print.noquote("Cramér V:")
  test <- "Cramér V"

	#return(as.numeric(CV))  

# --------------------------------------------------- #

# Find effect size for 2 x k tables:	
  
 # ES = c("tiny", "small", "medium", "large", NA)
  
  if (nd ==2 & CV < 0.1){
    ES <- "tiny"               
                }
   if (nd == 2 & 0.1 <= CV & CV < 0.3){
      ES <- "small"
      }
    if (nd == 2 & 0.3 <= CV & CV < 0.5){
      ES <- "medium"
      }
     if (nd == 2 & CV >= 0.5){
        ES <- "large"
        }

# -------------------------------------------------------------#

# Find effect size for larger tables

# 3:

  if (nd == 3 & CV < 0.71){
    ES <- "tiny"
    }
  
  if (nd == 3 & 0.71 <= CV & CV < 0.212){
    ES <- "small"
    }
  if (nd == 3 & 0.212 <= CV & CV < 0.354){
    ES <- "medium"
    }
   if (nd == 3 & CV >= 0.354){
    ES <- "large"
    } 
# 4:
   if (nd == 4 & CV < 0.058){
    ES <- "tiny"
    }
  
  if (nd == 4 & 0.058 <= CV & CV < 0.173){
    ES <- "small"
    }
  if (nd == 4 & 0.173 <= CV & CV < 0.289){
    ES <- "medium"
    }
   if (nd == 4 & CV >= 0.289){
    ES <- "large"
    } 
# 5:
  if (nd == 5 & CV < 0.05){
    ES <- "tiny"
    }
  
  if (nd == 5 & 0.05 <= CV & CV < 0.150){
    ES <- "small"
    }
  if (nd == 5 & 0.150 <= CV & CV < 0.250){
    ES <- "medium"
    }
   if (nd == 5 & CV >= 0.250){
    ES <- "large"
    } 

# 6:
   if (nd == 6 & CV < 0.045){
    ES <- "tiny"
    }
  
  if (nd == 6 & 0.045 <= CV & CV < 0.134){
    ES <- "small"
    }
  if (nd == 6 & 0.134 <= CV & CV < 0.224){
    ES <- "medium"
    }
   if (nd == 6 & CV >= 0.224){
    ES <- "large"
    } 

# 6 < :

if (nd > 6){
  ES <- NA
  }


        
# ------------------------------------------------------------------#
        
# get the results
res <- list(input.name, nd = nd, test = test, CV = CV, R2 = R2, ES = ES, DF = DF)


# -----------------------------------------------------------------#      
      # print results:  
        
    cat("\n",
     paste(rep("-",21)),"\n",
     sprintf("Effect size for contingency tables"),"\n",     
     sprintf("Data: %s", input.name) ,"\n",
      sprintf("df: %s", DF),"\n",
      sprintf("%s: %f", test, CV),"\n",
      sprintf("Pseudo R^2: %s", R2), "\n",
      sprintf("T-shirt effect size: %s", ES),"\n",
     
      paste(rep("-",21)),"\n",
      sprintf("Warning: effect sizes are only guidelines!"),"\n"
      
      )
      invisible(res)
      options(warn=0)   # return warnings to default
}

# ---------------------------------------------------------------- #


### Usage:

### >source("path\filename.r")
### >cramer.test(data)
### >V <- cramer.test(data)
### >V