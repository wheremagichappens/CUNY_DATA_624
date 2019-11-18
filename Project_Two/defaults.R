### UNIVERSAL DATA SOURCING & DEFAULT SETTINGS FOR PROJECT
library(knitr)
library(kableExtra)
library(default)
library(ggplot2)

# Set default augments for code chunks
knitr::opts_chunk$set(echo = F, message=F, warning=F, error=F, comment=NA, tidy=T, tidy.opts=list(width.cutoff=60), fig.width=10, fig.height = 3, fig.pos = 'H')

# wrap plot 
defOut <- knitr::knit_hooks$get("plot")  # save the default plot hook 
knitr::knit_hooks$set(plot = function(x, options) {  # set new plot hook ...
  x <- defOut(x, options)  # first apply the default hook
  if(!is.null(options$wrapfigure)) {  # then, if option wrapfigure is given ...
    # create the new opening string for the wrapfigure environment ...
    wf <- sprintf("\\begin{wrapfigure}{%s}{%g\\textwidth}", options$wrapfigure[[1]], options$wrapfigure[[2]])
    x  <- gsub("\\begin{figure}", wf, x, fixed = T)  # and replace the default one with it.
    x  <- gsub("{figure}", "{wrapfigure}", x, fixed = T)  # also replace the environment ending
  }
  return(x)
})

# Set default augments for `kable()` 
default(kable) <- list(format="latex")

# Set default augments for `kable_styling()` 
default(kable_styling)  <- list(latex_options = c("HOLD_position", "striped"), font_size = 8)
default(row_spec) <- list(row=0, bold=T)

# Set default augments for ggplot2 `theme()`
default(theme) <- list(axis.text.x = element_text(angle = 0, hjust = NULL),
                       plot.title = element_text(color="#0F52BA", size=10, face="bold", hjust = 0.5),
                       plot.subtitle = element_text(size=8, color="#868b8c"),
                       legend.title = element_text(size=8, color="#868b8c", face="bold"),
                       strip.background = element_rect(color="#000000", fill="#73C2FB", size=.25,linetype="solid"),
                       strip.text.x = element_text(size = 8, color = "#000000", face="bold", margin = margin(.05,0,.05,0, "cm")),
                       plot.caption = element_text(size = 6)
                       )
default(scale_x_continuous) <- list(labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))
default(scale_y_continuous) <- list(labels = scales::number_format(accuracy = 0.1,decimal.mark = '.'))
default(scale_fill_manual) <- list(values=c("#999999", "#95C8D8", "#008081", "#034D92"))