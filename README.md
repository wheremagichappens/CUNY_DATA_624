# GROUP TWO GUIDELINES

## MEAT & POTATOES:
*  Submissions should be completed in a timely manner, within group internal deadlines. 
*  Thoughtful feedback to all homework submissions must be provided in order to compile work. 
*  Responses to all questions should be answered thoroughly with explanations. 
*  Responses should be proofed and spell checked (F7 shortcut in R) upon completion. 
*  Insert all R libraries used in the library code chunk.
*  Only call plotting and formatting libraries as needed in the RMD to compile assignment 

## FORMATTING
*  UPDATE HOMEWORK YAML WITH NAME AND DATE COMPLETED ONLY 
*  UNIVERSAL LATEX FORMATTING WILL BE APPLIED TO THE FINAL SUBMISSION TO ENSURE EVERYONE CAN COMPILE DOCUMENT ON THEIR MACHINE
*  EACH DOCUMENT SHOULD BE KNITTED TO A PDF FOR EACH GROUP MEMBER TO REVIEW.
*  EVERYONE IS INDIVIDUALLY RESPONSIBLE FOR ENSURING THE FILE KNITS PROPERLY. 
*  DEFAULT FORMATTING HAS BEEN SET WITHIN EACH TEMPLATE.  

### TABLES: 
*  All table outputs should be wrapped using the default knitr and kable_styling settings: `%>% kable() %>% kable_styling() %>% row_spec()`
*  Add captions to table where appropriate: `kable(caption="CAPTION")`
### PLOTS:
*  fig.height` in code chunk options should be adjusted to larger size when needed (default=3)
*  All plots should be done using ggplots 
*  Lables should be used to appropriately when not included default graph: `+labs(title="", subtitle="", x="", y="")`
*  All plots should call `+theme_bw()+theme()` to apply default settings
