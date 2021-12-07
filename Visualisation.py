import netCDF4 as nc
import matplotlib.pyplot as plt
import matplotlib.animation as anim
import numpy as np

data = nc.Dataset("out/netcdf_output.nc", "r", format = "NETCDF4")
data.__dict__

position=data.variables["position"][:,:]
velocity=data.variables["velocity"][:,:]
acceleration=data.variables["acceleration"][:,:]
rho = data.variables["rho"][:,:]
phi = data.variables["phi"][:,:]
Ex = data.variables["Ex"][:,:]
Ey = data.variables["Ey"][:,:]

ims =[]
fig = plt.figure()
for k in range(0, position.shape[0]):
    im =[]
    im.append(plt.scatter(position[k,0], position[k,1], color = 'r')) #creating a scatter plot for each timestep to visualise the data
    im.append(plt.text(0.75, 0.75 , s = ("T = " + str(k)), ha = "center", weight = "bold"))
    # U = acceleration[k,0]; V = acceleration[k,1]
    # L = np.hypot(U, V)
    # im.append(plt.quiver(position[k,0], position[k,1], U, V, width = L*3, headlength = 2, headwidth = 3, headaxislength = 2, minlength = 0.01, units = 'inches', pivot = 'middle'))
    ims.append(im) #add to list of scatter plots
final_an = anim.ArtistAnimation(fig, ims, interval = 1, repeat = False) #creates animation out of list of scatter plots - 1 ms per frame
plt.gca().set_aspect('equal', adjustable='box')
plt.xlim(-1,1)
plt.ylim(-1,1)
plt.show()
