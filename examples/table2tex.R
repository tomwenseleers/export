# Create a file name
filen <- tempfile(pattern = "table_aov") # or 
# filen <- paste("YOUR_DIR/table_aov")

# Generate ANOVA output
fit=aov(yield ~ block + N * P + K, data = npk) # 'npk' dataset from base 'datasets'
x=summary(fit)

# Export to Latex in standAlone format
if (interactive()) table2tex(x=x,file=filen) 
# Export to Latex to paste in tex document
summary(fit) # get output from the console
if (interactive()) table2tex(file=filen, standAlone = FALSE) 

# Export to HTML
if (interactive()) table2html(x=x,file=filen) # or 
summary(fit) # get output from the console
if (interactive()) table2html(file=filen) 
