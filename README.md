# PX913 Miniproject


- Solves for the scalar potential from a given charge density
- Computes the trajectory of a charged particle moving through the potential
- Plots the resulting data


## Downloading/Building/Running
Cloning the repo:
```bash
git clone https://github.com/oaholroyd/px913-miniproject
```

The project can then be build using the Makefile provided (using `make` with any of the following flags: `all`, `test`, `clean`).

### Running the code
The code takes three (optional) command line options using a `key=value` structure (no spaces wither side of the equals):

- **nx** and **ny** specify the (positive integer) number of gridcells in the x and y dimensions
- **problem** takes a string keyword specifying the charge density (`null`, `single`, `double`).

eg
```bash
./particle nx=100 ny=120 problem=single
```

#### Output
The code outputs the following to a netCDF file:

- charge density (rho)
- computed potential (phi)
- x-derivative of the energy field (Ex)
- y-derivative of the energy field (Ey)
- timeseries of particle positions (x,y)
- timeseries of particle velocities (ux,uy)
- timeseries of particle accelerations (ax,ay)

#### Plotting
Once the data has been generated it can be plotted with
```bash
python3 plot.py output.nc
```
which generates a pseudocolour plot of Ex and a scatterplot of particle positions (x,y).
