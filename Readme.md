

```python
import matplotlib.pylab as plt, spinmob, subprocess, os, glob
```

# Simulating neutron stars

Results and experiments in simulating neutron stars with [MESA](http://mesa.sourceforge.net).

- Go through the Summer School project by prof. Andrew Cumming
- Simulate these different stars to make sure everything works fine:
    1. Neutron star with accretion rate of $2\times10^{-11}$
    2. Neutron star with diffusion, but no accretion
- Simulate a neutron star with accretion & diffusion (by Friday November 7)
- Simulate neutron stars with accretion rates between $2\times10^{-11}$ and $2\times10^{-10}$, with and without diffusion (by Friday November 14)
- Compare the following with results from the 2007 Peng paper (by Friday November 21)
    - Sedimentation & accretion velocities
    - Figures of one zone model


```python
# defined in Utilities/rn.py
def rn(path):
    currdir = os.getcwd()
    os.chdir(path)
    subprocess.call('./mk')
    subprocess.call('./rn')
    subprocess.call('images_to_movie.sh \'png/grid1_*.png\' mp4/grid1.mp4')
    os.chdir(currdir)
```

## Summer School Lecture

The lecture can [be found on the MESA website](http://mesastar.org/teaching-materials/2015-mesa-summer-school/cumming), and at `Summer school` in the repo.

### Up & running

#### What to get

- What is the approximate range of density and temperature in the model? What is the pressure scale height at the base? This gives you a rough estimate of the thickness of the layer we are simulating – only the very outer part of the neutron star!
- What is the recurrence time of the flashes?
- What is the peak luminosity? What do you think sets this physically?
- What fuel is burning in the flash?
- What happens to the CNO elements over time?
- What do you think will happen if you run the simulation for a long time?

### Burning regimes

1. Choose some accretion rate from $M=2\times10^{-11}$ to $2\times10^{-8}M_{sun}\mathrm{yr}^{-1}$

#### What to get

- What is happening to the CNO elements at different depths?
- What is the luminosity between bursts?
- Do all the bursts in a sequence look the same? How many bursts have to happen before the model gets into a steady state?
- How does the number of zones and the timestep change with time?


```python
MODEL = 'accretion_rate_model/'
OUT = 'accretion_rate_{}/'
def make_models(start=2e-8, end=2e-11, N=10, model=MODEL, out=OUT, run=False):
    start, end = plt.log(start), plt.log(end)
    accretion_rates = plt.exp(plt.linspace(start, end, N))
    with open(model+'inlist_ns_h', 'r') as model:
        model = model.read()
    for rate in accretion_rates:
        subprocess.call('cp -r {} {}'.format(model, out.format(rate)), shell=True)
        with open(out.format(rate)+'inlist_ns_h', 'w') as inlist:
            inlist.write(model.format(rate))
        if run: rn('{}'.format(out.format(rate)))
```

### Extras

- You tried changing the base luminosity Lb. The other parameters of the initial model are the mass and radius of the neutron star. Try changing M and R using the appro- priate relax controls in the inlist.
- The accreted material is assumed to have a metallicity of Z = 0.02 in the example above. What happens if you change Z?
- Run a model with a high M ̇ , so that the burning is stable. How does the helium mass fraction change with column depth once the helium starts to burn (hint: it should be a power law)? Does it make sense given what you know about the triple alpha reaction?
- The base of the model changes temperature significantly when a burst goes off. You might worry that we are not modelling a thick enough layer to include the high density material currently off the grid that gets heated up during the burst. How could you change the neutron star model so that it has a thicker envelope, say 100 times more massive than the one in the test suite?

### Nets! Comparison to GS1826-24

### A bigger net

#### What to get

- the recurrence time
- the time it took to run the model, and the amount of memory being used (to check the memory usage, run top and look for the star process)
- the peak luminosity
- with the burst peak luminosities normalized in the burst window, measure the time after the peak of the burst where the model lightcurve starts to deviate sig- nificantly from the GS1826 data (this is a way to quantify how well the lightcurve matches the data)
- the mass fraction of hydrogen in the ashes layer – i.e. how much hydrogen is present in the layer of heavy elements left over from previous bursts? You may want to change the axis limits on the abundance plot to help see this more clearly (e.g. zoom in on column depths between 8.0 and 8.5).

### Smaller and larger sets

### Adaptative nets

### Discussion

- Given the sensitivity to the choice of network that we’ve seen here, it should be clear that you have to be careful to choose the right network. How do you choose which net to use, and how can you be sure it is the right one for the problem you are interested in?
- What else would you do to make sure you are getting the right answer before you used the lightcurves from MESA to compare to data or to write a paper?
- We’ve seen that these models are pushing MESA to its limits, with hundreds of nuclei required to follow all the nucleosynthesis during the X-ray burst and calculations that take several hours or more per burst. What improvements, changes, or additions could we make to the MESA code to improve performance?

## Tests

### Accretion rate of $2\times10^{-11}$

### Diffusion only

## Accretion & diffusion

## Various accretion rates


```python
# make_models(...)
```

## Comparisons to figures & paper


```python

```
