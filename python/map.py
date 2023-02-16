## This code displays the map of Q+-Q- stored in map.out

# Import modules
from numpy import loadtxt
import numpy as np
import matplotlib.pyplot as plt
from matplotlib import colors

# Load data
my_data = loadtxt("./../output/scurve/map2.out", unpack=True)

# Reshape the values into 2D arrays of shape (nmap, nmap)
nmap=1000
one_radius= np.reshape(my_data,(nmap,nmap))

# Rotate the array so that the map corresponds to increasing T and Sigma
# For a better understanding, data is separated into its negative and positive values, hence two imshows and cmaps

plt.imshow(np.flip(np.rot90(-one_radius,-1),1),cmap='Blues', norm=colors.LogNorm(), origin='lower')
cbar1=plt.colorbar()
cbar1.set_label('valeurs négatives', rotation=270)

plt.imshow(np.flip(np.rot90(one_radius,-1),1),cmap='Reds', norm=colors.LogNorm(), origin='lower')
cbar2=plt.colorbar()
cbar2.set_label('valeurs positives', rotation=270)

# Legend
plt.xlabel('Sigma (u.a.)')
plt.ylabel('T (u.a.)')
plt.title('Q+-Q- (J/s/m^2)')

plt.show()