### UNIVERSAL DATA SOURCING & DEFAULT SETTINGS FOR PROJECT
library(knitr)
library(kableExtra)
library(default)
library(easypackages)

# Set default augments for code chunks
knitr::opts_chunk$set(echo = T, message=F, warning=F, error=F, comment=NA,self.contained = F, fig.width=10, fig.height = 3)

# Set default augments for `kable_styling()` 
default(kable) <- list(format="latex")
default(kable_styling)  <- list(latex_options = c("HOLD_position", "striped"))
default(row_spec) <- list(row=0, bold=T)