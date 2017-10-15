#' @title Multiple Linear Regression.
#' @description Multiple Linear Regression.
#' @param formula Contains dependent and independent variables for linear regression
#' @param data A data.frame to conduct linear regression
#' @exportClass linreg
#' @export linreg

linreg <- setRefClass("linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    reg_coef = "matrix",
    fitted_values = "matrix",
    resi = "vector",
    deg_free = "numeric",
    res_var = "numeric",
    var_reg_coef = "matrix",
    t_each_coef = "matrix",
    p_values = "matrix",
    data_name = "character"
  ),
   methods = list(
    initialize = function(formula, data){
      formula <<- formula
      data <<- data
      data_name <<- deparse(substitute(data))
      stopifnot((class(formula)=="formula") && (class(data)=="data.frame"))

  X <- model.matrix(formula, data)

  y_namn <- all.vars(expr = formula)[1]

  y <- (data[, y_namn])

  reg_coef <<- solve((t(X) %*% X)) %*% t(X) %*% y #

  fitted_values <<- X %*% reg_coef #y_hat

  resi <<-  as.vector(y - fitted_values) # e

  n <- length(y)
  p <- length(colnames(X))

  deg_free <<- n - p #

  res_var <<- as.numeric((t(resi) %*% resi) / deg_free) #

  var_reg_coef <<- res_var * solve((t(X) %*% X)) #

  t_each_coef <<- reg_coef / sqrt(diag(var_reg_coef)) #

  p_values <<- 2*pt(-abs(t_each_coef), df = deg_free)

  },

  print = function(){
    formula_str <- Reduce(paste, deparse(formula))
    cat("Call:\n")
    cat(paste("linreg(formula = ",formula_str,", data = ", data_name,")\n",sep=""))
    cat("Coefficients:\n")
    cat(" ",row.names(reg_coef), "\n    ", sep = "  ")
    cat(t(reg_coef), sep="    ")
  },
  plot= function() {
    form_temp <- as.character(formula)
    form <- paste("linreg(",form_temp[2],form_temp[1],form_temp[3],")")
    z <- data.frame(fitted_values,resi)
    colnames(z)<-c("fitted","residuals")
    p1<-ggplot(z, aes(x=fitted, y=residuals)) +
      geom_point(shape=1, size=2) +
      xlab(paste("Fitted values",form, sep="\n")) +
      ylab("Residuals") +
      ggtitle("Residuals vs Fitted") +
      geom_text(aes(label = tail(z$residuals,1), x=max(z$fitted), y=max(z$residuals)), hjust=1.5, size = 1)+
      stat_summary(aes(x=fitted,group=1),fun.y=median, colour="red", geom="line")+
      stat_summary(aes(x=fitted,group=1),fun.y=mean, colour="gray", geom="line",linetype=2)+  labs(caption="LiU") +
      theme(panel.background = element_rect(fill="white"),
            plot.margin = unit(c(1,1,1,1), "cm"),
            plot.caption = element_text(size=12, hjust=0.5, margin=margin(t=15),colour="blue"),
            plot.title = element_text(color="#666666", face="bold", size="15",hjust=0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color= "#666666", size=0.1),
            axis.text.x = element_text(color="#666666", size="5"),
            axis.text.y = element_text(color="#666666", size="5"),
            axis.title.x = element_text(color="#666666", size="12", face="bold"),
            axis.title.y = element_text(color="#666666", size="12", face="bold"),
            axis.ticks.x = element_line(color = "blue", size = 0.3))

    mod_residuals <- sqrt(abs(z$residuals / sqrt(res_var)))
    z[,3]<-mod_residuals
    colnames(z)[3] <- "mod_residuals"
    p2<-ggplot(z[,c(1,3)], aes(x=fitted, y=mod_residuals)) +
      geom_point(shape=1, size=2) +
      xlab(paste("Fitted values",form, sep="\n")) +
      ylab("sqrt(|Standardized residuals|)") +
      ggtitle("Scale-Location")+
      stat_summary(aes(x=fitted,group=1),fun.y=median, colour="red", geom="line")+
      labs(caption="LiU") +
      theme(panel.background = element_rect(fill="white"),
            plot.margin = unit(c(1,1,1,1), "cm"),
            plot.caption = element_text(size=12, hjust=0.5, margin=margin(t=15),colour="blue"),
            plot.title = element_text(color="#666666", face="bold", size="15",hjust=0.5),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.line = element_line(color= "#666666", size=0.1),
            axis.text.x = element_text(color="#666666", size="5"),
            axis.text.y = element_text(color="#666666", size="5"),
            axis.title.x = element_text(color="#666666", size="12", face="bold"),
            axis.title.y = element_text(color="#666666", size="12", face="bold"),
            axis.ticks.x = element_line(color = "blue", size = 0.3))
    return(gridExtra::grid.arrange(p1,p2,ncol=1,newpage=F))},

  resid = function(){return(resi)
  },

  pred = function(){
    return(as.vector(fitted_values))
  },

  coef = function(){
    return(reg_coef[,1])
  },

  summary = function(){

    library(ggplot2)
    tmp <- c(as.matrix(reg_coef[,1]),as.matrix(sqrt(diag(var_reg_coef))),t_each_coef,p_values)
    tmp <- matrix(tmp,nrow=length(reg_coef))
    tmp <- cbind(tmp,1:length(reg_coef))
    tmp<-round(tmp,4)
    tmp <- as.data.frame(tmp)
    p <-p_values
    rownames(tmp) <- row.names(reg_coef)
    for(i in 1:length(reg_coef[,1])){
      if(p[i]>=0&&p[i]<0.001){
        tmp[i,5]<-"***"
      }else if(
        p[i]>=0.001&&p[i]<0.01){
        tmp[i,5]<-"**"
      }else if(
        p[i]>=0.01&&p[i]<0.05){
        tmp[i,5]<-"*"
      }else if(
        p[i]>=0.05&&p[i]<0.1){
        tmp[i,5]<-"."
      }else if(
        p[i]>=0.1&&p[i]<1){
        tmp[i,5]<-""
      }
      tmp
    }


    colnames(tmp) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)","")

    tmp<-capture.output(tmp)
    cat("Call:","\n")
    cat(paste("linreg(formula = ",formula,", data = ",data_name,")",sep="")[3])
    cat("\n")
    cat("Coefficients:", "\n")
    for(i in 1:(length(formula)+1)){
      cat(paste(as.character(tmp[i]),collapse = " "))
      cat(sep = "\n")
    }
    cat(paste(
      'Residual standard error:',
      round(sqrt(res_var),4),
      'on',
      deg_free,
      'degrees of freedom'
    ))
    cat('\n')
  }
  ))



