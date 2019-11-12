# GROUP TWO GUIDELINES

## MEAT & POTATOES:
*  Submissions should be completed in a timely manner, within group internal deadlines. 
*  Thoughtful feedback to all homework submissions must be provided in order to compile work. 
*  Responses to all questions should be answered thoroughly with explanations. 
*  Responses should be proofed and spell checked (F7 shortcut in R) upon completion. 
*  Insert all R libraries used in the library code chunk.
*  Only call plotting and formatting libraries as needed in the RMD to compile assignment 

## FORMATTING:
*  Update homework YAML with name and date completed only 
*  Universal latex formatting will be applied to the final submission to ensure everyone can compile document on their machine
*  Each document should be knitted to a pdf for each group member to review.
*  Everyone is individually responsible for ensuring the file knits properly. 
*  Default formatting has been set within each template.  

#### TABLES: 
*  All table outputs should be wrapped using the default knitr and kable_styling settings: `%>% kable() %>% kable_styling() %>% row_spec()`
*  Add captions to table where appropriate: `kable(caption="CAPTION")`

#### PLOTS:
*  `fig.height` in code chunk options should be adjusted to larger size when needed (default=3)
*  All plots should be done using ggplots 
*  Lables should be used to appropriately when not included default graph: `+labs(title="", subtitle="", x="", y="")`
*  All plots should call `+theme_bw()+theme()` to apply default settings
