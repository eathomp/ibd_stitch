`ibd_stitch`
=============

`ibd_stitch` is a program for sampling identity by descent (IBD) among individuals in a population from genetic marker data.  While it is not a part of the 
 [MORGAN][morg] software suite, it was developed in the same research group and as a result uses similar configuration, input, and output file formats. In many cases this documentation will refer to MORGAN formats and documentation.

Edit test.

Running 
-------

The program is invoked as 

     ibd_stitch [parameter file]

All configuration for an analysis is specified in in the parameter file.

It is recommended to enable multicore processing with

     ibd_stitch [parameter file] +RTS -N

Output
-----------------
A single [MORGAN][morg] format IBD graph file containing realizations of IBD on the input chromosomes.

Configuration
-----------------

Parameters for a run of ibd\_stitch are given as statements in a text file.  There is no formal syntax for the various options.

* `input marker data file input.markers`   
Specifies the file of SNP data to use in the analysis.  This file comes in [MORGAN][morg] marker format; see that documentation or the example for more information.

* `output extra file ibdgraphs.out`  
The output file to be created or overwritten with the IBD graphs produced by ibd\_stitch.  
* `set iterations 1000`  
(Optional) The number of IBD graphs to generate if not building on pedigree IBD graphs. Either this option or `set priors` must be set.

* `set priors ["filename1","filename2","filename3"]`   
(Optional) The files containing the input pedigree IBD graphs for sampling.  The files should span disjoint subsets of the sample.  The total number of output IBD graphs will match the number in the shortest input pedigree file. Either this option or `set iterations` must be set.

* `set markers 5 10 15 20`  
(Required if using `set priors`) The marker indices at which to include pedigree IBD in the output graphs.  Pedigree IBD is likely to have been sampled at a lower density than the available markers, so it is enforced only at markers used in the pedigree sampling.

* `select [unphased|phased] data`  
Determines whether the input data will be modeled as unordered genotypes or phased haplotypes.


* `select population kinship 0.05`  
Sets the model's level of population kinship, or the probability of IBD between two alleles.


* `set kinship change rate 0.1`  
A scaling parameter for the change rate of the hidden Markov process.

* `set transition matrix null fraction 0.1`  
The percentage of the free transition matrix to be mixed with the the model transition matrix.  This is a tuning parameter controlling the scale of the uniformization approximation used to simplify computations.


* `set genotyping error rate 0.01`  
The model probability that a given allele is observed with error, in which case it is assumed to be drawn from the population allele distribution.


* `set seed 12345`   
The seed for the pseudorandom number generator.

Download 
-----------------

### Build from source
Building requires the [Haskell Platform](http://www.haskell.org/platform/).  Building has only been tested on Ubuntu 12.04 LTS, but the following steps should be cross-platform. There are other ways to install a cabal package, but this has worked best for me. Testing performed with GHC 7.4.1.

1. Ensure that you have the libraries `gsl` and `lapack` installed.  On Ubuntu this can be done with

         apt-get install libgsl0-dev
         apt-get install liblapack-dev

2. If it is not already installed, install `cabal-dev`

         cabal install cabal-dev

3. Clone the github repository, and move into the new directory 
         
         git clone https://github.com/cglazner/ibd_stitch

4. Build the cabal package, installing dependencies locally

         ~/.cabal/bin/cabal-dev install 

5. The executable can be found at `./cabal-dev/bin/ibd_stitch`


[morg]: http://www.stat.washington.edu/thompson/Genepi/MORGAN/Morgan.shtml "linku"
