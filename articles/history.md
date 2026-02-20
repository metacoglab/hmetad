# History of the hmetad package

The `hmetad` package is the most recent implementation of the meta-d’
model. However, this model has had several implementations since its
creation.

The first implementation of the model used maximum likelihood estimation
for single participant data ([Maniscalco and Lau
2012](#ref-maniscalco2012)), and is still available for download
[here](https://www.columbia.edu/~bsm2105/type2sdt/). The original code
is written in MATLAB, however an updated version is also available in
Python.

The model was later implemented by ([Fleming 2017](#ref-fleming2017)) in
a hierarchical Bayesian framework, which has been shown to provide much
more reliable estimates in the relatively small sample sizes commonly
used in psychological experiments. This version, known as the [Hmeta-d
toolbox](https://github.com/metacoglab/HMeta-d), was implemented in the
probabilistic programming language JAGS, which in turn has interfaces in
both MATLAB and in R.

The `hmetad` package builds on these previous versions through
implementation in the `brms` package in R, allowing for estimation with
complex regression designs. Additionally, `brms` uses the probabilistic
programming language Stan, which permits much more efficient sampling
with more reliable model convergence warnings. Because of its increased
efficiency and flexibility, the `hmetad` package is now the recommended
approach to fitting the meta-d’ model.

## References

Fleming, Stephen M. 2017. “HMeta-d: Hierarchical Bayesian Estimation of
Metacognitive Efficiency from Confidence Ratings.” *Neuroscience of
Consciousness* 2017 (1): nix007.

Maniscalco, Brian, and Hakwan Lau. 2012. “A Signal Detection Theoretic
Approach for Estimating Metacognitive Sensitivity from Confidence
Ratings.” *Consciousness and Cognition* 21 (1): 422–30.
