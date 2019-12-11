### UNIVERSAL DATA SOURCING & DEFAULT SETTINGS FOR PROJECT
library(knitr)
library(kableExtra)
library(default)
library(ggplot2)
library(easypackages)
library(formatR)

# Set default augments for code chunks
knitr::opts_chunk$set(echo = F, message=F, warning=F, error=F, tidy.opts=list(width.cutoff=80),tidy=TRUE, comment=NA, fig.width=10, fig.height = 3)

# Set default augments for `kable()` 
default(kable) <- list(format="latex", digits=4)

# Set default augments for `kable_styling()` 
default(kable_styling)  <- list(latex_options = c("HOLD_position", "striped"), font_size=8)
default(row_spec) <- list(row=0, bold=T)

# Set default augments for ggplot2 `theme()`
default(theme) <- list(axis.text.x = element_text(size=8),
                       axis.text.y = element_text(size=8),
                       axis.title.x = element_text(color="#7d7464", face="bold", size=10, angle = 0, hjust = NULL),
                       axis.title.y = element_text(color="#7d7464", face="bold", size=10),
                       plot.title = element_text(color="#745010", size=12, face="bold"),
                       plot.subtitle = (element_text(size=10, color="#868b8c")),
                       legend.title = (element_text(size=10, color="#745010", face="bold")),
                       strip.background = element_rect(color="#000000", 
                                                       fill="#eee9e1", size=.75,linetype="solid"),
                       strip.text.x = element_text(size = 8, color = "#000000", face="bold"))