#' Linear Regression
#' @name ridgereg
#' @title ridgereg
#' @field formula formula. y ~ x_1 + x_2 + ... + x_n.
#' @field data data.frame.
#' @description Returns the result of the Linear Regression
#' @export
#'
ridgereg <- setRefClass( Class = "ridgereg",
fields = list(formula = "formula",data = "data.frame",
parsedata = "character",
identity_matrix="matrix",
beta_ridge_QR ="matrix",ybar_ridge_QR="matrix"             ),
methods=list(
initialize= function(formula,data,lambda,normalise = TRUE){
    formula  <<- formula
    data <<- data
    parsedata <<- deparse(substitute(data))
    x <- model.matrix(formula, data)
    x<- ((x[,-1]-mean(x[,-1])) / sd(x[,-1]))

    x<-x-mean(x)/sd(x)
    x_norm<-x
    yy  <- all.vars(expr = formula)[1]
    y <- (data[, yy])
    identity_matrix<<- diag(ncol(x_norm))# x_norm %*% (t(x_norm))
    lambda<-lambda

    #using QR decompositon
    QR_ridge<- qr(x_norm)
    R<-qr.R(QR_ridge)
    Q<-qr.Q(QR_ridge)
    # beta_ridge<<-solve((t(x_norm) %*% x_norm)+ (lambda * identity_matrix) ) %*% t(x_norm) %*% y

    beta_ridge_QR<<-solve((t(R)%*%R) + lambda * identity_matrix) %*% t(Q %*% R) %*% y
    ybar_ridge_QR<<- (x_norm %*% beta_ridge_QR)

},

print_QR= function(){


    cat("\n","Call:","\n",
    paste("ridgereg(", "formula = ", formula[2]," ", formula[1], " ", formula[3],
    ", ", "data = ", parsedata, ", lambda = 0)",sep = "", collapse = "\n" ),
    "\n","Coefficients:","\n",
    paste(row.names(beta_ridge_QR),
    sep = "  ", collapse ="  " ),"\n",
    format(round(beta_ridge_QR,2), justify = "centre",width = 10))

},

predict_QR =function(){
    cat("\n \n Predicted values or fitted values using QR decomposition:","\n\n")
    return(as.vector(round(ybar_ridge_QR, 2)))
},

coef_QR = function(){
    # cat("\n \nRegressions coefficients using QR decomposition:","\n\n")
    # ridge_coef_QR <-as.vector(round(beta_ridge_QR,2))
    # names(ridge_coef_QR)<-c(row.names(beta_ridge_QR))
    return (beta_ridge_QR)
}

))
