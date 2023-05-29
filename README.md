# Convenient functions for vizualizing water quality data


# Install the package
In R, you must have the devtools package installed. Call

```r
install_github("coffm049/ogwtools")
```


# Function overview
So far, there are only a few functions, but this is a work in progress and will eventuall grow

- algaViz - vizualizes the proportion of algal phla over time, faceted by site ID
- waterColViz - vizualizes water quality data taken over a water column


# Further suggestions
If you have further suggestions, please make an issue

Got dependencies using 
```r
pack <- available.packages()
pack["ggplot2","Depends"]
```
