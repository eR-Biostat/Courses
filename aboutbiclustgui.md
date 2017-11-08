---
layout: page
title: About the BiclustGUI
subtitle: Installation, Info & Links
---

The BiclustGUI for R Commander
==============================

`RcmdrPlugin.BiclustGUI` is a R Commander Plug-In containing multiple methods to apply biclustering and visualise these results.

Installing the GUI
------------------

To install the **CRAN Release Version** (1.1.1), please use the following commands:

``` r
setRepositories(ind=c(1:5))
install.packages("RcmdrPlugin.BiclustGUI")
```

To install the **Development Version** from either the *R-Forge* or *GitHub* repository use

``` r
setRepositories(ind=c(1:5))
install.packages("RcmdrPlugin.BiclustGUI",repos="http://R-Forge.R-project.org")
```

or

``` r
setRepositories(ind=c(1:5))
install.packages("devtools") # If not yet installed on your R Version
devtools::install_github("hadley/devtools") # Only run this if your currently installed 
                                            # devtools version is <= 1.12 (recursive dependencies bug)

devtools::install_github("ewouddt/RcmdrPlugin.BiclustGUI")
```

One of the options above will install the GUI and its dependencies from both CRAN and Bioconductor. Should some issue arise with a package installation, please try to manual install them. The code for this can be found [here](https://ibiostat.be/online-resources/online-resources/biclustgui/biclustgui).

On the initial start-up of R Commander, you will probably be prompted to install some additional dependencies. This should not take too long!

To launch the GUI, use:

``` r
library(RcmdrPlugin.BiclustGUI)
```

More Info & Links
-----------------

-   *Introductory Blog on GitHub/R-Bloggers* (See [Blogpost](https://ewouddt.github.io/RcmdrPlugin.BiclustGUI/2016/09/27/biclustGUI/)).
-   *Detailed Guide* (See [Vignette](https://github.com/ewouddt/RcmdrPlugin.BiclustGUI/raw/master/vignettes/GuideBiclustGUI.pdf)).
-   *Shiny Application for Biclustering* ([Shiny Cloud](https://uhasselt.shinyapps.io/shiny-biclust/) + [Stand-Alone](https://ibiostat.be/online-resources/online-resources/biclustgui/shinyapp) + GitHub (Coming Soon))
-   *R-Forge Project Page* ([R-Forge](https://r-forge.r-project.org/R/?group_id=2104)).
-   *I-Biostat Page* ([I-Biostat](https://ibiostat.be/online-resources/online-resources/biclustgui/biclustgui)).

Currently Available Methods/Diagnostics
---------------------------------------

<table style="width:79%;">
<caption>All included biclustering and diagnostics packages in the BiclustGUI:</caption>
<colgroup>
<col width="15%" />
<col width="40%" />
<col width="23%" />
</colgroup>
<thead>
<tr class="header">
<th>R Package</th>
<th>Biclustering Method</th>
<th>Publication</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td><code>biclust</code></td>
<td>Plaid</td>
<td>Turnet <em>et al.</em>, 2005</td>
</tr>
<tr class="even">
<td><code>biclust</code></td>
<td><span class="math inline"><em>δ</em></span>-biclustering</td>
<td>Cheng and Church, 2000</td>
</tr>
<tr class="odd">
<td><code>biclust</code></td>
<td>X Motif</td>
<td>Murali and Kasif, 2003</td>
</tr>
<tr class="even">
<td><code>biclust</code></td>
<td>Spectral</td>
<td>Kluger <em>et al.</em>, 2003</td>
</tr>
<tr class="odd">
<td><code>biclust</code></td>
<td>QuestMotif</td>
<td>Kaiser, 2011</td>
</tr>
<tr class="even">
<td><code>biclust</code></td>
<td>Bimax</td>
<td>Prelic <em>et al</em>., 2006</td>
</tr>
<tr class="odd">
<td><code>fabia</code></td>
<td>FABIA</td>
<td>Hochreiter <em>et al.</em>, 2010</td>
</tr>
<tr class="even">
<td><code>isa2</code></td>
<td>The Iterative Signature Algorithm</td>
<td>Bergman <em>et al</em>., 2003</td>
</tr>
<tr class="odd">
<td><code>iBBiG</code></td>
<td>Iterative Binary Biclustering of Genesets</td>
<td>Gusenleitner <em>et al</em>., 2012</td>
</tr>
<tr class="even">
<td><code>rqubic</code></td>
<td>Qualitative Biclustering</td>
<td>Li <em>et al</em>., 2009</td>
</tr>
<tr class="odd">
<td><code>BicARE</code></td>
<td>Biclustering Analysis and Results Exploration</td>
<td>Gestraud and Barillot, 2014</td>
</tr>
<tr class="even">
<td><code>s4vd</code></td>
<td>SSVD (Sparse Singular Value Decomposition)</td>
<td>Lee <em>et al</em>., 2010</td>
</tr>
<tr class="odd">
<td><code>s4vd</code></td>
<td>S4VD (SSVD incorporating stability correction)</td>
<td>Sill <em>et al</em>., 2011</td>
</tr>
<tr class="even">
<td><code>BiBitR</code></td>
<td>Biclustering Algorithm for extracting bit-patterns from binary data-sets</td>
<td>Rodriguez-Baena <em>et al</em>., 2011</td>
</tr>
<tr class="odd">
<td><strong>R Package</strong></td>
<td><strong>Diagnostics</strong></td>
<td><strong>Publication</strong></td>
</tr>
<tr class="even">
<td><code>BcDiag</code></td>
<td>Bicluster Diagnostic Plots</td>
<td>Aregay <em>et al</em>., 2014</td>
</tr>
<tr class="odd">
<td><code>superbiclust</code></td>
<td>Generating Robust Biclusters from a Bicluster Set</td>
<td>Khamiakova, 2013</td>
</tr>
</tbody>
</table>
