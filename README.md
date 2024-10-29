
## scCompare: a web app for single-cell RNA sequencing dataset comparisons across multiple auto-immune diseases


Single-cell RNA sequencing (scRNA-seq) datasets have been widely used to identify the cell types and marker genes with pivotal roles in driving pathogenesis and progression of auto-immune diseases. Comparative analysis of different cell types or diseases across multiple scRNA-seq datasets can reveal the homogeneous and heterogeneous pathogenesis, and a comprehensive web-based comparison tool that could streamline this process is not yet available. Thus, we introduce scCompare, a web-based platform for scRNA-seq comparisons in autoimmune diseases. scCompare includes 2,125 differential gene lists from 100 scRNA-seq datasets in 22 auto-immune diseases and a query system supporting 170 standardized keywords from four attributes (disease, cell type, tissue, and treatment). scCompare also provides three modules enabling several comparative analysis and visualization options. geneQuery supports comparisons of queried genes across differential gene lists identified from multiple scRNA-seq datasets. DEGEnricher performs cell-type -specific enrichment analysis across studies based on a user-input gene list. DEGCompare allows interactive comparisons of multiple differential gene lists of many studies and performs pathway enrichment analyses.

<br/><br/>
![alt text](https://github.com/abbviegrc/scCompare/blob/main/scCompare.png?raw=true)

## <a href="https://github.com/abbviegrc/scCompare/blob/main/tutorial/Manual_scCompare.pdf">Tutorial</a>

## Run locally
scCompare runs in Shinyapps.io server that takes around 30 seconds to initialize each app for the first time access. To avoid any waiting time, users can also download the codes from Github and scCompare database from <a href="https://www.icloud.com/iclouddrive/013D9ewhOhNjj-5tErDEWnhow#IBDTransDB" target="_blank">https://www.icloud.com/iclouddrive/013D9ewhOhNjj-5tErDEWnhow#IBDTransDB</a> and run RShiny locally.

## Contract
Shweta Yadav - shweta.yadav@abbvie.com</br>
Jing Wang - wang.jing@abbvie.com
