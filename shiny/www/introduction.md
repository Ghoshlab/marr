#### Introduction

This app provides a user interface for the open-source R package `marr`, which 
implements the method **Maximum Rank Reproducibility (MaRR)**, a nonparametric 
approach that detects reproducible signals using a maximal rank statistic for 
high-dimensional biological data.

Instructions for using the app are provided as tooltips in the 'Example' tab. 
An interface to use the software with uploaded data is provided in the 
'Analysis' tab. For more information about the MaRR method and marr software, 
see below and the 'Further Information' tab.

#### About MaRR

Reproducibility is an on-going challenge with high-throughput technologies that
have been developed in the last two decades for quantifying a wide range of
biological processes. One of the main difficulties faced by researchers is the
variability of output across replicate experiments (Li et al. (2011)). 
In each high-throughput experiment (e.g., arrays,
sequencing, mass spectrometry), a large number of features are measured
simultaneously, and candidates are often subjected for follow-up statistical
analysis. We use the term features to refer to biological features (e.g.,
metabolites, genes) resulting from a high-throughput experiment in the rest of
this article.  When measurements show consistency across replicate experiments,
we define that measurement to be reproducible. Similarly, measurements that are
not consistent across replicates may be problematic and should be identified. In
this vignette, features that show consistency across high-dimensional replicate
experiments are termed reproducible and the ones that are not consistent are
termed irreproducible.   The reproducibility of a high-throughput experiment
primarily depends on the technical variables, such as run time, technical
replicates, laboratory operators and biological variables, such as healthy and
diseased subjects. A critical step toward making optimal design choices is to
assess how these biological and technical variables affect reproducibility
across replicate experiments (Talloen et al. (2010), Arvidsson et al. (2008)).

In this app, we introduce the marr procedure Philtron et al. (2018), referred to
as **Maximum Rank Reproducibility** (**MaRR**) to identify reproducible features
in high-throughput replicate experiments. 

The MaRR procedure was originally proposed to assess reproducibility of gene
ranks in replicate experiments. The `marr` R-package contains the `Marr()`
function, which calculates  a matrix of signals ($\text{irreproducible}=0$,
$\text{reproducible}=1$) with $M$ rows (total number of features) and $J$
columns ($J={I \choose 2}$) (replicate sample pairs ${I \choose 2}$), where
$J$ is the total possible number of sample pairs of replicate experiments.
We assign feature $m$ to be reproducible if a certain percentage signals
($100c_s\%$) are reproducible for pairwise combinations of replicate
experiments, i.e.,
if 

$$ \frac{{\sum_{i<~i'}{{{r_{m,{(i,i')}}}}}}}{J}>c_s, $$

such that, $c_s \in (0,1)$.

Similarly, we assign a sample pair $(i,~i')$ to be reproducible if a certain
percentage signals ($100c_m\%$) are reproducible across all features, i.e.,
if $$ \frac{\sum_{m}{{r{_{m,(i,i')}}}}}{M}>c_m, $$
such that, $c_m \in (0,1)$.

The reproducible signal matrix is shown in Figure 1 below.

<img src="Marr_schematic.png"
     alt="Signal Matrix"
     style="display: block; margin-left: auto; margin-right: auto; max-width: 747px; width: 100%; max-height: 421px; height: 100%;" />
     
