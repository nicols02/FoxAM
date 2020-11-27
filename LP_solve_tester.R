#LP_Solve tester tests out the implementation of LP_solve for ILPs.

#install LP solve package
#install.packages("lpSolveAPI")
library("lpSolveAPI")


#try example from here: http://www.math.clemson.edu/~mjs/courses/mthsc.440/integer.pdf
#i.e. max 8x1 + 11x2 + 6x3+ 4x4
# s.t. 5x1 + 7x2 + 4x3 +3x4 <= 14; x_j \in {0,1}

#this problem has solution :x1= 0; x2= x3= x4= 1; value 21

#see implementation code from: https://rpubs.com/nayefahmad/linear-programming
# see also warning on returning lpSolveAPI model objects here: http://web.mit.edu/lpsolve/doc/R.htm 
#in summary, use lpSolveAPI within an R function and don't return the lprec model object to avoid duplicate copies

### INPUT COEFFICIENTS MATRIX --------------------------------------------------

## Set the coefficients of the decision variables -> C [for the objective function]
C <- c(8, 11, 6, 4)

# Create constraint matrix A
A <- matrix(c(5, 7, 4, 3), nrow=1, byrow=TRUE) #add rows as necessary; same for B
# Right hand side for the constraints
B <- c(14)


### INITIALIZE CONSTRAINTS MATRIX AND SET TYPE OF DECISION VARIABLES-------------
# Set 4 constraints and 3 decision variables
lprec <- make.lp(nrow = 1, ncol = 4)
# Set the type of problem we are trying to solve
lp.control(lprec, sense="max")


## Set type of decision variables to binary
set.type(lprec, columns= 1:4, type=c("binary"))


### BUILD PROBLEM MATRICES/INITIALIZE MODEL ---------------------------------------------
# Set objective function coefficients vector C
set.objfn(lprec, C)


# Add constraints
add.constraint(lprec, A[1, ], "<=", B[1])

### SOLVE PROBLEM -----------------------------------------------------------------------
# Solve problem (for help, type ?solve.lpExtPtr)
solve(lprec)


### INTERROGATE OUTPUTS -----------------------------------------------------------------

# Get the decision variables values
get.variables(lprec)  #returns the correct value

# Get the value of the objective function
get.objective(lprec)  #correct value =21


# Note that the default boundaries on the decision variable are c(0, 0, 0) and c(Inf, Inf, Inf)... for this problem we specified the variable type as binary so it's 0-1
get.bounds(lprec)
# Boundaries can be set with following function
#lpSolveAPI::set.bounds()