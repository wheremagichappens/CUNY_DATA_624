### UNIVERSAL DATA SOURCING & DEFAULT SETTINGS FOR PROJECT
library(knitr)
library(kableExtra)
library(default)
library(ggplot2)
library(easypackages)

# Set default augments for code chunks
knitr::opts_chunk$set(echo = T, message=F, warning=F, error=F, comment=NA,self.contained = F, tidy=T, tidy.opts=list(width.cutoff=50), fig.width=10, fig.height = 3)

# Set default augments for `kable()` 
default(kable) <- list(format="latex")

# Set default augments for `kable_styling()` 
default(kable_styling)  <- list(latex_options = c("HOLD_position", "striped"))
default(row_spec) <- list(row=0, bold=T)

# Set default augments for ggplot2 `theme()`
default(theme) <- list(axis.text.x = element_text(angle = 0, hjust = NULL),
                       plot.title = element_text(color="#2f818a", size=12, face="bold"),
                       plot.subtitle = (element_text(size=10, color="#868b8c")),
                       legend.title = (element_text(size=10, color="#4aa0b2", face="bold")),
                       strip.background = element_rect(color="#000000", 
                                                       fill="#51a36f", size=.75,linetype="solid"),
                       strip.text.x = element_text(size = 8, color = "#FFFFFF", face="bold"))