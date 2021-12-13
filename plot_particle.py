"""Plots the output of particle.f90"""

import matplotlib.pyplot as plt
from matplotlib import colors
from mpl_toolkits.axes_grid1 import make_axes_locatable
import numpy as np
import netCDF4 as nc


# read data from file
data = nc.Dataset("out/netcdf_output.nc", "r", format="NETCDF4")
pos = data["position"][:]
vel = data["velocity"][:]
acc = data["acceleration"][:]
rho = data["rho"][:]
phi = data["phi"][:]
Ex = data["Ex"][:]
Ey = data["Ey"][:]


# plot position as scatter plot
fig = plt.figure(num=1, figsize=(8, 7))
fig.suptitle("Particle Trajectory", fontsize=16)
ax = fig.add_subplot(1, 1, 1)
ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_xlim(-1, 1)
ax.set_ylim(-1, 1)

# plot and save
ax.scatter(pos[:, 0], pos[:, 1], 0.5, 'r')
fig.savefig("out/pos.png")


# plot Ex as a pseudocolor plot
fig = plt.figure(num=2, figsize=(8, 7))
fig.suptitle("Ex", fontsize=16)
ax = fig.add_subplot(1, 1, 1)
ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_xlim(-1, 1)
ax.set_ylim(-1, 1)

# add axis for the key
divider = make_axes_locatable(ax)
axc = divider.append_axes("right", size="5%", pad=0.05)

# plot and save
# (note that the y-axis has to be reversed)
im = ax.imshow(Ex, extent=[-1, 1, 1, -1])
cbar = plt.colorbar(im, axc)
fig.savefig("out/ex.png")


# plot trajectory over phi
fig = plt.figure(num=3, figsize=(8, 7))
fig.suptitle("phi", fontsize=16)
ax = fig.add_subplot(1, 1, 1)
ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_xlim(-1, 1)
ax.set_ylim(-1, 1)

# add axis for the key
divider = make_axes_locatable(ax)
axc = divider.append_axes("right", size="5%", pad=0.05)

# plot and save
im = ax.imshow(phi, extent=[-1, 1, 1, -1])
cbar = plt.colorbar(im, axc)
ax.plot(pos[:, 0], pos[:, 1], 'r-')
fig.savefig("out/phi.png")
