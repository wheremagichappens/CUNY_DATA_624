### UNIVERSAL DATA SOURCING & DEFAULT SETTINGS FOR PROJECT
library(knitr)
library(kableExtra)
library(default)
library(easypackages)

# Set default augments for code chunks
knitr::opts_chunk$set(echo = T, message=F, warning=F, error=F, comment=NA,self.contained = F, fig.width=10, fig.height = 3)

# Set default augments for `kable()` 
default(kable) <- list(format="latex")
# Set default augments for `kable_styling()` 
default(kable_styling)  <- list(latex_options = c("HOLD_position", "striped"))
default(row_spec) <- list(row=0, bold=T)

# Set default augments for ggplot2 `theme()`
default(theme) <- list(axis.text.x = element_text(angle = 0, hjust = NULL),
                       plot.title = element_text(color="#4c4c4c", size=12, face="bold"),
                       plot.subtitle = (element_text(size=8, color="#000000")),
                       legend.title = (element_text(size=10, color="#000000", face="bold")),
                       strip.background = element_rect(color="#000000", 
                                                       fill="#cccdd0", size=.75,linetype="solid"),
                       strip.text.x = element_text(size = 8, color = "#000000", face="bold"))