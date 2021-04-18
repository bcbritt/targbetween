# targbetween: Targeted Betweenness Centrality
Computes targeted betweenness centrality as described in Britt et al. (2021).

If you use this package, please cite the original manuscript:

Britt, B. C., Hayes, J. L., Musaev, A., Sheinidashtegol, P., Parrott, S., & Albright, D. L. (2021). Using targeted betweenness centrality to identify bridges to neglected users in the Twitter conversation on veteran suicide. _Social Network Analysis and Mining, 11_, article 40. Retrieved from https://doi.org/10.1007/s13278-021-00747-x



## Installation

To use this package, first install and load it in R with

```r
install.packages("devtools")
library(devtools)
install_github("bcbritt/targbetween")
library("targbetween")
```



## Usage

To compute targeted betweenness centrality, you need to indicate the network to be analyzed and the dyads that will be evaluated. There are two basic approaches depending on the nature of the dyads to be evaluated:

1. If the set of dyads consist of one of more "source" vertices and one or more "destination" vertices, and each of your source vertices shares a dyad with every destination vertex, then use `targbetween_alldyads`, specifying the set of "source" and "destination" vertices to be evaluated. An appropriate set of dyads for this analysis might look like the following:

```r
Source Destination
Ashley Diana
Ashley Emily
Bart   Diana
Bart   Emily
Chris  Diana
Chris  Emily
```

2. If you cannot define a set of "source" and "destination" vertices such that every source vertex shares a dyad with every destination vertex, then use the more general (but much less efficient) `targbetween` function instead. This approach is necessary if the set of dyads to be evaluated is more like the following:

```r
Source Destination
Ashley Bart
Ashley Chris
Bart   Ashley
Chris  Diana
Diana  Ashley
Diana  Emily
```



### Required Arguments

You can call each of these two functions as follows:

- `targbetween_alldyads` requires a network graph formatted in accordance with the [**igraph** package](https://igraph.org/r/), as well as a vector of one or more source vertices and a vector of one or more destination vertices. This function will perform targeted betweenness centrality, evaluating all possible dyads connecting the source vertices with the destination vertices. For instance, `targbetween_alldyads(my_network, c("Ashley","Bart","Chris"), c("Diana","Emily"))` would evaluate targeted betweenness centrality for the following dyads:

```r
Source Destination
Ashley Diana
Ashley Emily
Bart   Diana
Bart   Emily
Chris  Diana
Chris  Emily
```

- `targbetween` takes an [**igraph**](https://igraph.org/r/)-formatted network graph and a data.frame of dyads as required arguments. This provides maximum freedom, as you can specify any combination of dyads that you want. However, this function becomes very slow when evaluating a large number of dyads, so `targbetween_alldyads` should be used when possible. Regardless, here is a brief example on the use of `targbetween` when necessary:

```r
dyads <- rbind(c("Ashley","Bart"),c("Ashley","Chris"),c("Bart","Ashley"),c("Chris","Diana"),c("Diana","Ashley"),c("Diana","Emily"),)
targbetween(my_network, dyads)
```

The above code block will compute targeted betweenness centrality for the `my_network` network graph using the following set of dyads:

```r
Source Destination
Ashley Bart
Ashley Chris
Bart   Ashley
Chris  Diana
Diana  Ashley
Diana  Emily
```



### Optional Arguments

- Both `targbetween_alldyads` and `targbetween` can also accept three optional arguments:

  - Passing a valid file path as `filename` will cause either function to output its results to that location as a .csv. This argument defaults to `NULL`, which does not output the results to a file.

  - The `directed` argument allows you to indicate whether the network should be treated as directed. This argument defaults to `FALSE`, indicating that the network should be treated as being undirected.

  - Since the calculations may take considerable time for larger networks or larger sets of dyads, you can specify an `update` argument to provide a progress update periodically. Setting `update = 100`, for example, will print an update every 100 iterations of the major loops that are used in the analysis. Note, however, that this does not provide verbose output for the call to `igraph::all_shortest_paths`, which can take considerable time in its own right.

- Thus, the relevant function calls may be expanded, such as what you see in the below examples:

```r
targbetween_alldyads(my_network, c("Ashley","Bart","Chris"), c("Diana","Emily"), filename = "C:/Users/Admin/Desktop/output.csv")
targbetween(my_network, dyads, directed = TRUE, update = 1000)
```
