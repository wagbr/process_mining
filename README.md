# Process Mining discovery algorithms

In this repo, I will store some own algorithms for process mining
discovery models. Feel free to use them for your projects and please,
let me know of your improvements.

## Alpha miner

This is my own implementation of the Alpha Miner algorithm.

``` {r}
require(DiagrammeR) 
require(dplyr) 
require(tidyr)
require(plyr) 
require(bupaR)

log <- patients
```

``` {r}
graph <- alpha_miner(log) 
graph
```

