---
layout: post
title: Introduction to BiclustGUI
tags: ['R', 'Rcmdr','biclustering']
categories: ['R']
---

BiclustGUI in R
================
Ewoud De Troyer, University of Hasselt (CenStat)

Biclustering in a GUI & Growing a GUI together!
-----------------------------------------------

### Introduction

We are happy to announce the first release of the **BiclustGUI** on [CRAN](https://cran.r-project.org/web/packages/RcmdrPlugin.BiclustGUI/){:target="_blank"} (`RcmdrPlugin.BiclustGUI`)!
This GUI will you enable to quickly try out a wide arrange of biclustering algorithms and produce some helpfull graphs in order to explore your data. Since we made the choice of developing it in the form of a plug-in for R Commander, you can save your R code after the session which can be used without GUI intervention. However for those of you who love using Shiny, have no fear! We have also created a Shiny App including all the biclustering algorithms and the most interesting plots. (For all available diagnostics and graphs, you will have to head over to the BiclustGUI package itself though.)

This blog is meant as a short introduction to the GUI which will highlight some features, give an example or two and showcase what else we have done in this area. A detailed instruction guide about all aspects of the GUI (as well as some short explanation about the included algorithms) can be found in the form of a vignette [here](https://cran.r-project.org/web/packages/RcmdrPlugin.BiclustGUI/vignettes/GuideBiclustGUI.pdf){:target="_blank"}. [1]


### What is biclustering?

If you are familiar with the concept of biclustering, you can safely skip this section. If you are not, biclustering is actually a fairly easy concept! Say you have a matrix \(M\), instead of just clustering on a single dimension such as the columns (= finding similar columns based on all the rows), you are going to cluster *simultaneously* on both dimensions. This means you are trying to discover a subset of columns which are similar on only a subset of rows or vice versa. This submatrix which contains this local pattern is what we call a **bicluster**.
There exist a great deal of different algorithms to find these submatrices. For example, they can differ in which assumption they have of the pattern inside the bicluster (constant/evolution or additive/multiplicative). They can be based on a specific model, random initialisations, the type of data (binary/continuous),... too much to sum up here! In the GUI we have tried to include a large variety of these different methods.


<img src="{{ site.url }}/img/biclustGUI_images/unnamed-chunk-1-1.png" alt="Heatmap of Example Data Matrix with 2 Biclusters"  />
<p class="caption">
Heatmap of Example Data Matrix with 2 Biclusters
</p>

### The BiclustGUI R Package

Let me start with some motivation why we decided to create this GUI and what we tried to achieve.
As is with many data analysis workflows, there exist a lot of different R packages for biclustering. Since plowing through all of the reference manuals to discover all the functions and their arguments can be quite time-consuming, one of our goals was to alleviate this process. This is why we choose to create the BiclustGUI as a **R Commander plug-in**. On one side you have the easy point-and-click environment, and on the other side your R code, which uses the functions from the corresponding packages, is being generated. We hope by doing this, that the GUI is a helpful tool to quickly get into some biclustering exploration, while not limiting you to play around with the code after its initial generation.

When developing the GUI, we also wanted to provide a **unified platform** from which all of these packages are *connected* and can be *accessed* (both for the methods and diagnostics packages). The table at the end of this section shows which ones are included so far and the Examples section will show you the default look of a biclustering window.
This means that as long as you are using the interface, you do not have to worry about which R output object goes where in which argument of another function, even if the functions for the method or plot come from different packages. The generated R code will reflect this correctly.

The final aim, but not less important, was to create a **growing GUI**. Since new biclustering methods/diagnostics and packages are still being developed, ee required a way so that the GUI could grow along side these new developments.
The way we tackled this was by including easy *"fill-out"-scripts* which developers can use to create a GUI window themselves for their novel method. As soon as the window fits their needs, they can send us this script so that we can include the new method in the next release of the GUI with minimal work from our side.
I'll talk about more about these scripts in a later section, but the general gist is that you will have to fill out some general information about your method and then copy-paste which widgets should be used in your window! These scripts should be fairly easy to get an understanding of since the `tcltk` syntax has been completely omitted from them.

<table style="width:79%;">
<caption>All included biclustering and diagnostics packages in the BiclustGUI:</caption>
<colgroup>
<col width="15%" />
<col width="40%" />
<col width="23%" />
</colgroup>
<thead>
<tr class="header">
<th align="left">R Package</th>
<th align="left">Biclustering Method</th>
<th align="left">Publication</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td align="left"><code>biclust</code></td>
<td align="left">Plaid</td>
<td align="left">Turnet <em>et al.</em>, 2005</td>
</tr>
<tr class="even">
<td align="left"><code>biclust</code></td>
<td align="left"><span class="math inline"><em>δ</em></span>-biclustering</td>
<td align="left">Cheng and Church, 2000</td>
</tr>
<tr class="odd">
<td align="left"><code>biclust</code></td>
<td align="left">X Motif</td>
<td align="left">Murali and Kasif, 2003</td>
</tr>
<tr class="even">
<td align="left"><code>biclust</code></td>
<td align="left">Spectral</td>
<td align="left">Kluger <em>et al.</em>, 2003</td>
</tr>
<tr class="odd">
<td align="left"><code>biclust</code></td>
<td align="left">QuestMotif</td>
<td align="left">Kaiser, 2011</td>
</tr>
<tr class="even">
<td align="left"><code>biclust</code></td>
<td align="left">Bimax</td>
<td align="left">Prelic <em>et al</em>., 2006</td>
</tr>
<tr class="odd">
<td align="left"><code>fabia</code></td>
<td align="left">FABIA</td>
<td align="left">Hochreiter <em>et al.</em>, 2010</td>
</tr>
<tr class="even">
<td align="left"><code>isa2</code></td>
<td align="left">The Iterative Signature Algorithm</td>
<td align="left">Bergman <em>et al</em>., 2003</td>
</tr>
<tr class="odd">
<td align="left"><code>iBBiG</code></td>
<td align="left">Iterative Binary BIclustering of Genesets</td>
<td align="left">Gusenleitner <em>et al</em>., 2012</td>
</tr>
<tr class="even">
<td align="left"><code>rqubic</code></td>
<td align="left">Qualitative Biclustering</td>
<td align="left">Li <em>et al</em>., 2009</td>
</tr>
<tr class="odd">
<td align="left"><code>BicARE</code></td>
<td align="left">Biclustering Analysis and Results Exploration</td>
<td align="left">Gestraud and Barillot, 2014</td>
</tr>
<tr class="even">
<td align="left"><code>s4vd</code></td>
<td align="left">SSVD (Sparse Singular Value Decomposition)</td>
<td align="left">Lee <em>et al</em>., 2010</td>
</tr>
<tr class="odd">
<td align="left"><code>s4vd</code></td>
<td align="left">S4VD (SSVD incorporating stability correction)</td>
<td align="left">Sill <em>et al</em>., 2011</td>
</tr>
<tr class="even">
<td align="left"><strong>R Package</strong></td>
<td align="left"><strong>Diagnostics</strong></td>
<td align="left"><strong>Publication</strong></td>
</tr>
<tr class="odd">
<td align="left"><code>BcDiag</code></td>
<td align="left">Bicluster Diagnostic Plots</td>
<td align="left">Aregay <em>et al</em>., 2014</td>
</tr>
<tr class="even">
<td align="left"><code>superbiclust</code></td>
<td align="left">Generating Robust Biclusters from a Bicluster Set</td>
<td align="left">Khamiakova, 2013</td>
</tr>
</tbody>
</table>

### Installing the GUI

Before we continue to some examples, let's install the GUI. To do so, please follow the code below:

``` r
setRepositories(ind=c(1:5))
install.packages("RcmdrPlugin.BiclustGUI")
```

This will install the GUI and its dependencies from both CRAN and Bioconductor. Should some issue arise with a package installation, please try to manual install them. The code for this can be found [here](https://ibiostat.be/online-resources/online-resources/biclustgui/biclustgui){:target="_blank"}.

On the initial start-up of R Commander, you will probably be prompted to install some additional dependencies. This should not take too long!

To launch the GUI, use:

``` r
library(RcmdrPlugin.BiclustGUI)
```

(Note: A development version can be found on [GitHub](https://github.com/ewouddt/RcmdrPlugin.BiclustGUI) and [R-Forge](https://r-forge.r-project.org/R/?group_id=2104){:target="_blank"}).

### What's R Commander & The BiclustGUI Plug-in?

For those not familiar with R Commander (`Rcmdr`), it is a basic statistics GUI by John Fox based on the `tlctk` package. The basic R Commander window contains a script window, where the R code is generated, as well as a console and warning window. The BiclustGUI extends the R Commander window with two extra biclustering menus as you can see in the Figure below:

<img src="{{ site.url }}/img/biclustGUI_images/rcmdrandmenus.png" alt="Basic R Commander Window and Plug-in Menu" width="450" />
<p class="caption">
Basic R Commander Window and Plug-in Menu
</p>

### Examples

All biclustering windows follow the same structure (see Figure below) of 2 tabs.

-   The first tab, the **Biclustering Tab**, includes all parameters, a seed box (if required) and a *Show Results* button to apply the algorithm.
-   The second tab, the **Plots & Diagnostics Tab**, will contain, as the name suggests, *plots and diagnostics* for the chosen methods.
-   At the bottom of the second tab, there are also buttons to access more **general diagnostics packages**. Currently `BcDiag` and `superbiclust` are included.

<img src="{{ site.url }}/img/biclustGUI_images/standard_buttons.png" alt="Standard Window" width="300" />
<p class="caption">
Standard Window
</p>

Let's now look at 2 examples which mimic a short biclustering exploration. Both examples will show the general workflow you can expect when using the BiclustGUI

#### Plaid Example

1.  Apply Plaid with the chosen parameters.
2.  Go the second tab.
3.  Generate a Profile Plot.

<img src="{{ site.url }}/img/biclustGUI_images/guiexample.png" alt="Plaid Windows" width="900" />
<p class="caption">
Plaid Windows
</p>

We could have also accessed the *BcDiag* button on the bottom of the second tab to create the following profile plots:

<img src="{{ site.url }}/img/biclustGUI_images/bcdiagexample.png" alt="BcDiag Profile Plots" width="450" />
<p class="caption">
BcDiag Profile Plots
</p>

#### FABIA Example

1.  Apply FABIA with the chosen parameters. The corresponding R-code will be generated in the script window of R Commander.
2.  Go to the second tab.
3.  Use *Biclust Plots* button to access plots from the `biclust` package.
4.  Use the heatmap button to generate the graph in a R Graphics Device.

<img src="{{ site.url }}/img/biclustGUI_images/guiexample2.png" alt="FABIA Windows" width="900" />
<p class="caption">
FABIA Windows
</p>

### How about future algorithms? Including new methods!

As I explained in an earlier section, we wanted to develop an easy way to include new biclustering R packages in the future. We wanted to make sure the GUI stayed up to date with recent developments by growing it as a community. The way we addressed this problem this was through *"fill-out template scripts"* which do not require the original `tcltk` syntax. I will try to outline the general idea and workings behind these scripts, but by no means will this be a full tutorial! A much more detailed explanation can be found in the second part of the [vignette](https://cran.r-project.org/web/packages/RcmdrPlugin.BiclustGUI/vignettes/GuideBiclustGUI.pdf){:target="_blank"}.

The remainder of this section will be a bit more 'R-code dense' so if this does not interest you, you can safely skip to the next one, *The Shiny App*.

#### Window Function & Window Scripts

In `Rcmdr` or `tcltk` a window is simply a R function. Inside this function the window, interface and all its elements are defined. Calling this function will make the window appear in your R session. The script we provide will help you make such a function (e.g. `newmethod_WINDOW()`) in which the GUI is defined. This newly created window function can then be used by R Commander to call it from a meanu.

Before we head over to such a "fill-out" script, I should briefly mention that there are actually 2 types of them:

1.  The first type is used to created a biclustering window (with its 2 tabs, biclustering and plots & diagnostics).
2.  The second type is used to create an extra tool window. This can serve as an extension of the first (e.g. linking tool window functions with buttons) or a general diagnostic window.

Both scripts are nearly completely identical. They only differ in the fact that the first type is for the two tabs (biclustering and plots) window while the second type is only for a window with one tab which follows the same rules as the second tab in the first script.
Since they are so similar, let's just focus on the first type.

#### Structure of a Window Script

The script, provided in `newmethod_script.R` in the `doc` folder of the BiclustGUI package, starts with opening the window function (here called `newmethod_WINDOW`, but you can change this). Next, some objects are initialized, followed by some variables which need to changed to adapt it to the method you want to implement. These are variables such as the name of the method, the function used for the method, the argument of this function which accepts the data, etc.
It is also possible to add some discretize or binarize frames to the window (see Figure below). (See the [vignette](https://cran.r-project.org/web/packages/RcmdrPlugin.BiclustGUI/vignettes/GuideBiclustGUI.pdf){:target="_blank"} for more detailed information about all of these variables!)

What follows next is the information that will decide how the window will look like for the two tabs, but we will come back to that in a minute.
At the very end of the script all the defined variables come together in the `cluster_template()` function which will translate all of this info to `tcltk` syntax.

``` r
newmethod_WINDOW <- function(){
  
  new.frames <- .initialize.new.frames()
  grid.config <- .initialize.grid.config()
  grid.rows <- .initialize.grid.rows()
  
  #####################################################
  ## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
  #####################################################
  methodname <- "A new method"
  methodfunction <- "methodfunction"
  data.arg <- "d"
  data.matrix <- TRUE
  methodshow <- TRUE
  other.arg <- ""
  methodhelp <- ""
  # Extra Data Conversion Boxes
  data.discr <- FALSE
  data.bin <- FALSE
  # Possibility to give a seed ?
  methodseed <- TRUE
  
  ## COMPATIBILITY? ##
  # BcDiag
  bcdiag.comp <- FALSE
  # SuperBiclust
  superbiclust.comp <- FALSE
  
  ##########################
  #### BICLUSTERING TAB ####
  ##########################
  
  # OMITTEN FOR NOW (SEE FURTHER)
  
  
  #################################
  #### PLOTS & DIAGNOSTICS TAB ####
  #################################
  
  # OMITTED FOR NOW (SEE FURTHER)
  
  
  ###############################################################
  ## USE THE ARGUMENTS IN THE GENERAL CLUSTERTEMPLATE FUNCTION ##
  ###############################################################
  cluster_template(methodname = methodname, methodfunction =
    methodfunction, methodhelp = methodhelp, data.arg = data.arg,
    other.arg = other.arg, methodseed = methodseed,
    grid.config = grid.config, grid.rows = grid.rows,
    new.frames = new.frames, superbiclust.comp =
    superbiclust.comp, bcdiag.comp = bcdiag.comp, data.transf =
    data.transf, data.discr = data.discr, data.bin = data.bin,
    methodshow = methodshow, methodsave = methodsave)
}
```

<img src="{{ site.url }}/img/biclustGUI_images/discrbin.png" alt="Discretize and Binarize Frames" width="450" />
<p class="caption">
Discretize and Binarize Frames
</p>

#### Designing the Window (of the 2 tabs)

So the only part that's left now is to design how the two tabs should look like. Both tabs are created in the exact same way and follow **3 easy steps**:

1.  Making the frames.
2.  Configuring the frames into a grid (matrix).
3.  Combining rows into a box.

<img src="{{ site.url }}/img/biclustGUI_images/plaid_structure.png" width="900" style="display: block; margin: auto;" />

Let's look how this would look like for the first tab. Note that this part starts by putting the `input` variable to `"clusterTab"` to indicate that we are adding information to the first tab.

``` r
########################
#### CLUSTERING TAB ####
########################
input <- "clusterTab"

### 1. ADDING THE FRAMES ###

# Add frames here

### 2. CONFIGURING THE GRID ###
grid.config <- .grid.matrix(input=input,c("frame1","frame2",
    "frame3",NA,"frame4",NA),
    nrow=3,ncol=2,byrow=TRUE,grid.config=grid.config)

### 3. COMBING THE ROWS ###
grid.rows <- .combine.rows(input=input,rows=c(1),title="A nice
    box: ",border=TRUE,grid.rows=grid.rows,
    grid.config=grid.config)
grid.rows <- .combine.rows(input=input,rows=c(2,3),
    title="A nice box:",border=TRUE,grid.rows=grid.rows,
    grid.config=grid.config)
```

**Step 1**
In the first step we create the frames for the method function arguments. A variety of frames can be created:

-   Check Boxes
-   Radio Buttons
-   Entry Fields
-   Sliders
-   Spinboxes

The easiest way to create these is to open the `frames_script.R` file in the `doc` folder of the BiclustGUI and simply copy paste the default version of one of these frames and then adapt the variables as you need them (frame name, argument names, initial values, title, border,...). Later in this section you can find a short example of how to do this.
For example the default script of the entry fields frame and its result looks like:

<img src="{{ site.url }}/img/biclustGUI_images/entryfields.png" width="450" style="display: block; margin: auto;" />

It should be noted that in the second tab, plots & diagnostics, another type of frame can be added, namely *manual buttons*. These are used to call functions which calculate diagnostics, draw graphs or apply any other function you want.
The default script and result for this frame looks like:

<img src="{{ site.url }}/img/biclustGUI_images/manualbutton.png" width="450" style="display: block; margin: auto;" />

I would like to point attention to the `buttonfunction` and `arg.frames` variable. The first determines which function is tied to this button, the second determines which frames (meaning which arguments), are tied to this function/button.

**Step 2**
Next you need to take the frame names of the frames defined in *step 1* and put them into a matrix with the `.grid.matrix()` function which is saved in the `grid.config` object.
The way the names are placed in the matrix will decide how they will appear in the window (although note that frames will always be pulled towards the top-left).
Note that apart from some extra arguments which should not be changed, `grid.matrix()` accepts the same input as `matrix()`.

**Step 3**
The final step allows you to group together/put a box or title around 1 or multiple rows of the earlier defined matrix. This helps to add visual distinction between parts of the window. Its other use is to prevent all frames from trying to fit in 1 general grid which make frames jump too much to the right sometimes. Putting them in a "box of rows" will make a subgrid in which they will try to fit.
To this end the function `.combine.rows()` is used and the result is saved the `grid.rows` object. The only arguments which should be changed are `rows`, `title` and `border`. Note that this can be done multiple times to put multiple rows in different boxes.

Making the second tab will be completly similar to steps described with the only differences being the `input` variable set to `"plotdiagTab"` and the option of adding `manual buttons`.

#### Workflow of creating of a new method window

This is how a general workflow of creating a new window would look like:

1.  Open `newmethod_script.R` and start adjusted the information variables in the beginning (as well as the name of `newmethod_WINDOW`).
2.  Open `frames_script.R` and start applying the 3 window steps for both tabs while copy pasting default frames from `frames_script.R`
3.  Run the window function while the BiclustGUI is launched and check if the design is correct.
4.  Send your new method addition to the maintainer of the BiclustGUI:
    -   The script(s) of your new window(s).
    -   A function that transforms the output of your biclustering algorithm to the `Biclust-class` in the `biclust` package. This is a S4 object in which the most important slots are `RowxNumber`, `NumberxCol` and `Number`. Providing this transformation function will make sure both the functions from `BcDiag` and `superbiclust` immediately work with your new method in the framework of the GUI.

#### Window creation examples

In the BiclustGUI, all included biclustering and diagnostics packages are already implemented in the GUI by using these scripts. So for more examples, you could always take inspiration from the source code of the GUI, since they will follow the same script as explained above. The vignette itself also includes some more examples.

To finalize this probably way too elaborate section about window creating, let's take a look at some excerpts of the script which created the Plaid window!

The script starts with adapting the general information so that the variables fit for the Plaid algorithm.

``` r
#####################################################
## GENERAL INFORMATION ABOUT THE NEW METHOD/WINDOW ##
#####################################################
methodname <- "Plaid"
methodfunction <- "biclust"
data.arg <- "x"
data.matrix <- TRUE
other.arg <- ",method=BCPlaid()"
methodhelp <- "BCPlaid"
methodseed <- TRUE
data.discr <- FALSE
data.bin <- FALSE
bcdiag.comp <- TRUE
superbiclust.comp <- TRUE
```

Next, we go on to step 1 of the first tab, defining the frames. In the figure below you can find 2 examples of these frames and how it would look like in the final interface.

<img src="{{ site.url }}/img/biclustGUI_images/plaid_clusterbuild.png" width="600" style="display: block; margin: auto;" />

Once step 1 is completed, we head over to step 2 and 3 to configure the grid and create 2 boxes around row 1 and row 2 and 3.

``` r
### 2. CONFIGURING THE GRID ###
grid.config <- .grid.matrix(input=input,c("toclusterframe",
    "modelframe","backgroundcheckframe",NA,
    "backgroundentryframe1","backgroundentryframe2"),byrow=
    TRUE,nrow=3,ncol=2,grid.config=grid.config)
### 3. COMBING THE ROWS ###
grid.rows <- .combine.rows(input=input,rows=c(1),title=
    "Plaid Specifications",border=TRUE,grid.rows=grid.rows,
    grid.config=grid.config)
grid.rows <- .combine.rows(input=input,rows=c(2,3),title=
    "Layer Specifications",border=TRUE,grid.rows=grid.rows,
    grid.config=grid.config)
```

The same 3 steps are repeated for the second tab which will now include buttons as well. The following figure shows how the heatmap button pulls its arguments from 2 different frames, a checkbox and entry field.

<img src="{{ site.url }}/img/biclustGUI_images/plaid_plotdiagbuild.png" width="600" style="display: block; margin: auto;" />

At the end everything comes together again in the `cluster_template()` function which marks the end of the script.

### The Shiny App

For those not interested in the R-code behind the biclustering algorithms or maybe for those who would like to introduce non-statisticians to biclustering, we have also created a shiny application which includes all of the currently implemented biclustering methods in the BiclustGUI.
It is possible to draw heatmaps, profile plots and export your results. However for other diagnostics or the `superbiclust` method (= combining many biclustering results into robust biclusters), you will have to fall back to the original BiclustGUI in R Commander. (We do plan to include superbiclust in some form in the future.)

The Shiny App is available:

1.  online on the [Shiny Cloud](https://uhasselt.shinyapps.io/shiny-biclust/) (currently only with limited resources!){:target="_blank"}.
2.  as a **stand-alone version** right [here](https://ibiostat.be/online-resources/online-resources/biclustgui/shinyapp){:target="_blank"} ([Direct Link](https://ibiostat.be/online-resources/online-resources/biclustgui/biclust-shiny-standalone/biclustshiny_standalone_1-0.1){:target="_blank"} for version 1.0.1).
    -   Download the zip-file, extract and doubleclick `LAUNCH.vbs`.

<img src="{{ site.url }}/img/biclustGUI_images/shinyibbig.png" alt="Shiny App - iBBiG" width="900" />
<p class="caption">
Shiny App - iBBiG
</p>

### The REST Package

A side-product of the BiclustGUI project came in the form of another R package, [REST](https://cran.r-project.org/web/packages/REST/index.html){:target="_blank"} (**R**cmdr **E**asy **S**cript **T**emplates), a **tool to create R Commander GUI Plug-in's**. It is currently also available on CRAN.
For this package, we basically took the template scripts (which were specific for biclustering), generalized them and added some extra functionality.

This package is by no means as flexible or powerful as `shiny`, but it does provide you a quick, easy and no-nonsense way to create a **R Commander plug-in** for your own analysis or R package while not having to bother about any `tlctk` syntax.

Essentialy it would be possible to completely recreate the BiclustGUI using the REST package.

### Book Project

For those interested in more information on the Biclust GUI, I refer to the elaborate [vignette](https://cran.r-project.org/web/packages/RcmdrPlugin.BiclustGUI/vignettes/GuideBiclustGUI.pdf){:target="_blank"} included in the BiclustGUI package.

The GUI will also be featured in the upcoming book *Applied Biclustering Methods for Big and High Dimensional Data Using R* by Kasim, A., Shkedy, Z., Kaiser, S., Hochreiter, S. and Talloen, W. (28th of September 2016). The book handles various applications of biclustering in gene expression experiments, chemoinformatics, molecular modelling, etc. More information can be found [here](https://ibiostat.be/online-resources/online-resources/biclustgui/bookproject){:target="_blank"}.

### Contact

Please direct any **questions/suggestions/bugs** to `ewoud.detroyer[at]uhasselt.be`.

We are happy to take any feedback!

[1] Currently the vignette on CRAN still shows version 1.0.4. A more updated vignette can be found inside the package itself in version 1.0.6.
