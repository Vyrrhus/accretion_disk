import disk
import numpy as np
from matplotlib import pyplot as plt
from matplotlib import cm
from scipy.spatial import cKDTree
import matplotlib.animation as animation
from time import time
import argparse

# Timer
def timer(func):
    """Get runtime of decorated function"""
    def wrapper(*args, **kwargs):
        tIni = time()
        result = func(*args, **kwargs)
        tEnd = time()
        print(f"Fonction {func.__name__} exécutée en {tEnd-tIni:.4f}s")
        return result
    return wrapper

# Data => blur => imshow
# Gaussian blur
def data_coord2view_coord(p, resolution, pmin, pmax):
    dp = pmax - pmin
    dv = (p - pmin) / dp * resolution
    return dv

def kNN2DDens(xv, yv, resolution, neighbours, dim=2):
    """
    """
    # Create the tree
    tree = cKDTree(np.array([xv, yv]).T)
    # Find the closest nnmax-1 neighbors (first entry is the point itself)
    grid = np.mgrid[0:resolution, 0:resolution].T.reshape(resolution**2, dim)
    dists = tree.query(grid, neighbours)
    # Inverse of the sum of distances to each grid point.
    inv_sum_dists = 1. / dists[0].sum(1)

    # Reshape
    im = inv_sum_dists.reshape(resolution, resolution)
    return im

def plot_cloud(simulation, count, xlims, angle=(45,0,0), resolution=500, neighbours=150):
    xs, ys = Projection3D(angle).project(simulation)
    resolution = 500
    neighbours = 150

    extent = list(xlims) + list(xlims)
    xv = data_coord2view_coord(xs, resolution, extent[0], extent[1])
    yv = data_coord2view_coord(ys, resolution, extent[2], extent[3])

    fig = plt.figure(figsize=(10,10), facecolor="black")
    fig.subplots_adjust(top=1, bottom=0, left=0, right=1)
    ax = fig.add_subplot(111)
    ax.set_facecolor("black")
    ax.set_axis_off()

    if neighbours == 0:
        ax.plot(xs, ys, 'k.', markersize=5)
        ax.set_aspect('equal')
        ax.set_title("Scatter Plot")
    else:
        im = kNN2DDens(xv, yv, resolution, neighbours)
        ax.imshow(im, origin='lower', extent=extent, cmap=cm.inferno, vmin=np.min(simulation.temperature), vmax=np.max(simulation.temperature))
        ax.set_xlim(extent[0], extent[1])
        ax.set_ylim(extent[2], extent[3])
    
    fig.savefig(f'frame_array/frame_{count:05d}.png', dpi=150, bbox_inches='tight')
    print(f"Fig n° : {count} saved", end="\r")
    plt.close()

# Coordinates
class Frame():
    def __init__(self, data, z_scale=1, size=1000):
        # Data
        self.data = data

        # Time
        self.time  = data.time
        self.time0 = 0

        #####################
        # Modèle 1D
        self.radius_max     = data.space
        self.radius_min     = np.zeros_like(data.space)
        self.radius_min[0]  = 3
        self.radius_min[1:] = data.space[0:-1]

        self.demi_hauteur = data.get("H").T / data.constantes["R_S"] / z_scale
        self.densite_surf = data.get("SIGMA").T
        self.temperature  = data.get("TEMP").T

        #####################
        # Dataset
        self.bins        = None
        self.r2          = None
        self.angle0      = None
        self.angle       = None
        self.vitesse_ang = None
        self.altitude    = None

        # Coordonnées cartésiennes + scatter color
        self.x     = None
        self.y     = None
        self.z     = None
        self.color = None
        self.alpha = None

        # Crée le dataset
        self.init_points(size)

        #####################
        # Plot
        self.scatter = None
        self.index   = -1

    def init_points(self, size):
        """ Le volume est défini par N bins de volume, en coordonnées cylindriques:
            - self.radius_min et self.radius_max de rayon
            - 0 à 2 pi d'angle
            - - self.demi_hauteur et self.demi-hauteur d'altitude

            La loi de densité pour répartir les point est donnée par self.densite_surf
        """
        # ID des bins
        bins_ID = np.arange(self.data.space.shape[0])

        # Attribution des points à chaque bin pour suivre la densité de surface
        self.bins = np.random.choice(bins_ID, p=self.densite_surf[:,1055] / np.sum(self.densite_surf[:,1055]), size=size)

        # Rayon
        self.r2 = np.random.uniform(self.radius_min[self.bins]**2, self.radius_max[self.bins]**2)

        # Altitude (distribution uniforme)
        self.altitude = np.random.uniform(-self.demi_hauteur[:,1055][self.bins], self.demi_hauteur[:,1055][self.bins])

        # Angle initial (distribution uniforme)
        self.angle0    = np.random.uniform(0, 2*np.pi, size)
        self.angle_ini = self.angle0
        
        # Vitesse angulaire, angle & coordonnées cartésiennes
        self.compute(0)

        # Couleurs & alpha
        temp_min   = np.array([self.temperature[0,1055]] + list(self.temperature[:-1,1055]))
        temp_max   = self.temperature[:,1055]
        self.color = np.random.uniform(temp_min[self.bins], temp_max[self.bins]) / np.max(self.temperature)

        sigma_min  = np.array([self.densite_surf[0,1055]] + list(self.densite_surf[:-1,1055]))
        sigma_max  = self.densite_surf[:,1055]
        self.alpha = np.random.uniform(sigma_min[self.bins], sigma_max[self.bins]) / np.max(self.densite_surf)

    def compute(self, time):
        # Vitesse angulaire
        self.vitesse_ang = (self.data.constantes["MASS"] * 6.6743e-11 / (self.altitude**2 + self.r2)**(3/2))**0.5
        self.angle       = self.vitesse_ang * (time - self.time0) + self.angle_ini

        # Coordonnées cartésiennes
        self.x = self.r2**0.5 * np.cos(self.angle)
        self.y = self.r2**0.5 * np.sin(self.angle)
        self.z = self.altitude * np.cos(self.angle - self.angle0)

    def update(self, time, dT_array):
        """ Pour dT_array la durée caractéristique entre chaque pas de temps du modèle 1D
        """
        duree = np.array([0] + list(np.cumsum(dT_array)))
        idx = np.argwhere(time >= duree)[-1][0]

        # Changement d'index
        if idx != self.index:
            if idx == self.data.time.shape[0] - 1: exit()
            print(f"Bin : {idx}/{self.data.time.shape[0]} - t = {self.data.time[idx]:.5f}s")
            # Index du bin temporel en cours
            self.index = idx

            # Reset des valeurs initiales
            self.time0  = time
            self.angle_ini = self.angle

            # Bornes pour l'altitude
            self.altitude_ini = self.altitude
            self.altitude_end = np.random.uniform(0, self.demi_hauteur[:,idx+1][self.bins]) * np.sign(self.altitude)

            # Bornes température
            temp_min = np.array([self.temperature[0,idx]] + list(self.temperature[:-1,idx]))
            temp_max = self.temperature[:,idx]
            self.color_ini = self.color
            self.color_end = np.random.uniform(temp_min[self.bins], temp_max[self.bins]) / np.max(self.temperature)

            # Bornes sigma
            sigma_min  = np.array([self.densite_surf[0,idx]] + list(self.densite_surf[:-1,idx]))
            sigma_max  = self.densite_surf[:,idx]
            self.alpha_ini = self.alpha
            self.alpha_end = np.random.uniform(sigma_min[self.bins], sigma_max[self.bins]) / np.max(self.densite_surf)

        # Altitude
        self.altitude = self.altitude_ini + (self.altitude_end - self.altitude_ini) / dT_array[idx] * (time - self.time0)

        # Vitesse angulaire, angle & coordonnées cartésiennes
        self.compute(time)

        # Température & alpha
        self.color = self.color_ini + (self.color_end - self.color_ini) / dT_array[idx] * (time - self.time0)
        self.alpha = self.alpha_ini + (self.alpha_end - self.alpha_ini) / dT_array[idx] * (time - self.time0)

    def plot(self, ax, angle=(0,0,0), **kwargs):
        x, y = Projection3D(angle).project(self)
        self.scatter = ax.scatter(x, y, s=1, c=self.color, cmap="magma", vmin=0, vmax=1, **kwargs)

class Projection3D():
    def __init__(self, angles:tuple):
        # Angles
        self.angles = np.array(angles) * np.pi / 180
        angleX, angleY, angleZ = self.angles

        # Projection 2D
        self.projection_2D_matrix = np.array([
            [1,0,0],
            [0,1,0],
            [0,0,0]
        ])
        
        # Rotation matrix (eulerian lol)
        self.rotationX = np.array([
            [1, 0, 0],
            [0, np.cos(angleX), -np.sin(angleX)],
            [0, np.sin(angleX), np.cos(angleX)]
        ])

        self.rotationY = np.array([
            [np.cos(angleY), 0, np.sin(angleY)],
            [0, 1, 0],
            [-np.sin(angleY), 0, np.cos(angleY)]
        ])

        self.rotationZ = np.array([
            [np.cos(angleZ), -np.sin(angleZ), 0],
            [np.sin(angleZ), np.cos(angleZ), 0],
            [0,0,1]
        ])
    
    def project(self, data:Frame):
        data3D = np.vstack((data.x, data.y, data.z))

        rotation = np.eye(3)
        if self.angles[0]:
            rotation = rotation @ self.rotationX
        if self.angles[1]:
            rotation = rotation @ self.rotationY
        if self.angles[2]:
            rotation = rotation @ self.rotationZ

        view = self.projection_2D_matrix @ rotation @ data3D
        return view[0,:], view[1,:]

class Projection2D():
    def __init__(self, angles:tuple):
        # Angles
        self.angles = np.array(angles) * np.pi / 180
        angleX, angleY, angleZ = self.angles
        
        # Rotation matrix (eulerian lol)
        self.rotationX = np.array([
            [1, 0],
            [0, np.cos(angleX)]
        ])

        self.rotationY = np.array([
            [np.cos(angleY), 0],
            [0, 1]
        ])

        self.rotationZ = np.array([
            [np.cos(angleZ), -np.sin(angleZ)],
            [np.sin(angleZ), np.cos(angleZ)]
        ])
    
    def project(self, data:Frame):
        data2D = np.vstack((data.x, data.y))

        rotation = np.eye(2)
        if self.angles[0]:
            rotation = rotation @ self.rotationX
        if self.angles[1]:
            rotation = rotation @ self.rotationY
        if self.angles[2]:
            rotation = rotation @ self.rotationZ

        view = rotation @ data2D
        return view[0,:], view[1,:]

##########################################################################################################################
#       ANIMATION PLOT
#

if __name__ == "__main__":
    # Parser
    parser = argparse.ArgumentParser(
        description="Affiche les données obtenues par la simulation.",
        allow_abbrev=True)
    parser.add_argument("output",
                         help="Nom du fichier de sortie (Ex: [data.out])")

    args = parser.parse_args()

    data = disk.DataHandler(args.output)

    #####################################################
    # Figure & ax

    fig = plt.figure(figsize=(8,8), facecolor="black")
    fig.subplots_adjust(top=1, bottom=0, left=0, right=1)
    ax = fig.add_subplot(111)
    ax.set_facecolor("black")
    ax.set_axis_off()

    # Limits
    lims = (-np.max(data.space) * 1.1, np.max(data.space) * 1.1)
    ax.set_xlim(*lims)
    ax.set_ylim(*lims)

    #################################################
    # Initialisation
    idx_min = 0
    idx_max = -1
    data.time = data.time[idx_min:idx_max]
    data.df   = data.df.loc[data.time[0]:data.time[-1],:,:]

    maxFrame = 10000
    dT       = 1e-9
    dTarray  = 1e-7 * np.ones_like(data.time)
    dTarray  = (data.time[1:] - data.time[:-1]) / np.max(data.time) * 1e-3
    dTarray  = np.where(dTarray < 1e-7, 5e-9, 1e-8)
    simu     = Frame(data, z_scale=1/1, size=10000)
    simu.time = 0
    simu.plot(ax)

    x = np.floor(simu.x).astype(int)
    y = np.floor(simu.y).astype(int)

    xy = np.zeros((simu.x.shape[0],2))
    xy[:,0] = x
    xy[:,1] = y

    a = xy

    def update(frameNumber, obj):
        print(f"{frameNumber*100}/{maxFrame}", end="\r")
        obj.scatter.remove()
        obj.update(obj.time, dTarray)
        obj.plot(ax, angle=(45,0,0))
        # plot_cloud(obj, frameNumber, lims)
        obj.time += dT

    anim = animation.FuncAnimation(fig, update, fargs=(simu,), frames=maxFrame, interval=10)

    plt.show()