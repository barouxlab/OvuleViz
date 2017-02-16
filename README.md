# OvuleViz
Graphical interface to analyse segmented cell data

v1.2 (14/02/2017)

#### To start the analysis run (from an R console):

```R
library(shiny)
runGitHub('OvuleViz', 'barouxlab')
```

#### To create a segmented data file from Imaris output run (from an R console):

```R
library(RCurl)
u <- 'https://raw.githubusercontent.com/barouxlab/OvuleViz/master/getData.R'
eval(parse(text = getURL(u, ssl.verifypeer = FALSE)))
```

## Notes

### Data selection

The plotted data can be subsetted on the primary identifiers 'Genotype', 'Cell Labels', 'Stages' and on secondary tags (e.g. 'SMC neighbour cells'). The levels for each identifier are automatically retrieved from the file with segmented data.

### Viewpoints

Different cell types ('Labels') can be combined into 1-3 'viewpoints'. The Labels in each Viewpoint are mutually exclusive (i.e. the same label cannot be used simultaneously in two different viewpoints).

### Y-axis

The different measured variables (e.g 'Cell Volume') are retrieved automatically from the file with segmented data. In addition, the extra variable 'Cell Number' is generated, representing the count of each cell type ('Label') or viewpoint in each stack.

### Data mapping onto plots

Each data point in the plots represents:

* one individual cell, if one of the variables present in the the segmented data file is in use (e.g 'Cell Volume')

* one individual stack, if the variable 'Cell Number' is in use.

The subsetted data can be grouped in different subplots ('Split plots on') and in different colors ('Color on'). The available variables for grouping will vary according to the type of plot, etc. The same data point will never be used twice in the same plot (including its subplots).

For every selected set of data, four main types of plots can be generated:

- boxplots represent the median, the 25% and 75% quartiles, the highest value within 1.5 times the inter-quartile range (whiskers), and any outliers.

- scatter plot represent the individual data points. The interactive version of the scatter plot can be used to identify individual data points.

- the histogram can represent the actual data counts, or a normalized value (density) - this is useful when comparing groups with unequal sizes. It is advisable to try different numbers of bins as this will strongly affect the plot distribution.

- The values on the 'means' tab represent the mean ± standard error of the mean.

### p-values

A p-value can be optionally represented on each subplot ('More options' menu). It is calculated using a Kruskal–Wallis test between **groups defined by color**. This means that *all the values with the same color in each subplot will be pooled*, and that the X-axis is not taken into consideration.

The [Kruskal–Wallis test](https://en.wikipedia.org/wiki/Kruskal%E2%80%93Wallis_one-way_analysis_of_variance) (also known as one-way ANOVA on ranks) is a non-parametric alternative to the standard one-way ANOVA. Unlike the latter, it does not assume a normal distribution of the data. When used with two groups, it is equivalent to a [Mann–Whitney–Wilcoxon test](https://en.wikipedia.org/wiki/Mann%E2%80%93Whitney_U_test). An intuitive way to understand this test is that while the regular ANOVA (and t-tests) compare means, the Kruskal-Wallis (and Mann-Whitney-Wilcoxon) compare medians. A low p-value indicates that at least one population median is different from the population median of at least one other group.

Note: if any of the subplots has only values from a single color (and therefore no comparison is possible), none of the other p-values will be calculated. In that situtation, remove the data points that are present in those subplots (e.g. remove specific stages)


### Further options

- The overall height of the plot should be manually set using the slider control. The width of the plot can be adjusted by dragging the browser window.

- The range of the Y-axis can be manually set. This will act as a 'zoom', and will leave out of the plot values which are outside that range.

- The y-axis can be represented on a log scale (which may useful to compare values across large differences of magnitude)

- The color scheme used to distinguish different groups (e.g. Genotypes) can be set to one of the ['Color Brewer' schemes](http://colorbrewer2.org/)

- The color scheme will optionally use a custom Cell Types set.

- The background of the plots can be set to grey to optimize contrast.



### Data download
- All the values that are currently selected can be viewed as a table (and downloaded as a csv file) from the 'table' tab. Note that although this table can be filtered and sorted on the web browser, the downloaded file and the other plots will not be affected.

- The means + se values can be viewed as a table and downloaded directly from the respective tab. Again, the plot (and downloaded csv file) will not be affected by filtering / sorting made directly on this table.

- The raw data (i.e. the data present in the segmented data file) can be inspected on the 'Data > Original data' page.

- A summary of the number of Stacks available for each Stage and Genotype can be seen in the 'Data > Total Stacks' page.

## Segmented data file creation

The script incorporates a few filters, including restricting the type of 'Labels' (e.g 'L1 apical') and 'Type' (e.g. 'Cell Area'). The list with allowed values can be updated by setting the 'TrueLabel' and 'TrueType' lists *before* running the GitHub script.

E.g. to add the new cell type 'Novel Cell', run:

```r
TrueLabel <- c("", "CC", "L1 apical", "L1 basal", "L1 basal sup", "L1 dome", "L2 apical", "L2 basal", "L2 basal sup", "pSMC", "SMC", "Novel Cell")
```

For more details, check the screen output after running the script.
