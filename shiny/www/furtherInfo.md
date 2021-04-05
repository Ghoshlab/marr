#### Installing marr Software

The R package `marr` is made available on 
[Github](https://github.com/Ghoshlab/marr) and 
[Bioconductor](https://bioconductor.org/packages/release/bioc/html/marr.html).

`marr` can be installed from GitHub with the R package
[devtools](https://github.com/hadley/devtools) using the following R code:

    if (!require("devtools")) install.packages("devtools")
    devtools::install_github("Ghoshlab/marr")
    
The latest release version can also be installed via Bioconductor: 

```s
# install BiocManager from CRAN (if not already installed)
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")

# install marr package
BiocManager::install("marr")
```

After installation, the package can be loaded into R.
```s
library(marr)
```

---

#### MaRR Method

The **MaRR** paper published in *Journal of American Statistical
Association*:

> Philtron, Daisy, et al. “Maximum Rank Reproducibility: A Nonparametric
> Approach to Assessing Reproducibility in Replicate Experiments.”
> Journal of the American Statistical Association 113.523 (2018):
> 1028-1039. <https://doi.org/10.1080/01621459.2017.1397521>

**Paper (in preparation)**
> Ghosh, Tusharkanti, et al. “Reproducibility of Mass Spectrometry based
> Metabolomics Data”

---

#### Bug reports

Report bugs as issues on the [GitHub repository new
issue](https://github.com/Ghoshlab/marr/issues/new)

---

#### Contributors

-   [Tusharkanti Ghosh](https://github.com/tghosh30)
-   [Max McGrath](https://github.com/max-mcgrath)
-   [Daisy Philtron]()
-   [Katerina Kechris](http://csph.ucdenver.edu/Sites/Kechris/)
-   [Debashis Ghosh](https://github.com/ghoshd)

---


#### References

Arvidsson, Samuel, Miroslaw Kwasniewski, Diego Mauricio Riaño-Pachón, and 
Bernd Mueller-Roeber. 2008. “QuantPrime–a Flexible Tool for Reliable 
High-Throughput Primer Design for Quantitative Pcr.” 
*BMC Bioinformatics* 9 (1): 465.

Li, Qunhua, James B Brown, Haiyan Huang, Peter J Bickel, and others. 2011. 
“Measuring Reproducibility of High-Throughput Experiments.” 
*The Annals of Applied Statistics* 5 (3): 1752–79.

Talloen, Willem, Sepp Hochreiter, Luc Bijnens, Adetayo Kasim, Ziv Shkedy, 
Dhammika Amaratunga, and Hinrich Göhlmann. 2010. “Filtering Data from 
High-Throughput Experiments Based on Measurement Reliability.” 
*Proceedings of the National Academy of Sciences* 107 (46): E173–E174.