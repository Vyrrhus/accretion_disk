import numpy as np
from matplotlib.figure import Figure
from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import (FigureCanvasTkAgg, NavigationToolbar2Tk)
import matplotlib.animation as animation
import argparse

import tkinter
from tkinter import ttk
import pandas as pd

from time import time
import io

#============================================================
# MATPLOTLIB GLOBAL SETTINGS

def init_plotting():
    plt.rcParams['figure.figsize'] = (8, 5)
    plt.rcParams['font.size'] = 12
    # plt.rcParams['font.family'] = 'Times New Roman'
    plt.rcParams['axes.labelsize'] = plt.rcParams['font.size']
    plt.rcParams['axes.titlesize'] = 1.5*plt.rcParams['font.size']
    plt.rcParams['legend.fontsize'] = plt.rcParams['font.size']
    plt.rcParams["legend.title_fontsize"] = plt.rcParams['font.size']
    plt.rcParams['xtick.labelsize'] = plt.rcParams['font.size']
    plt.rcParams['ytick.labelsize'] = plt.rcParams['font.size']
    # plt.rcParams['savefig.dpi'] = 2*plt.rcParams['savefig.dpi']
    plt.rcParams['xtick.major.size'] = 3
    plt.rcParams['xtick.minor.size'] = 3
    plt.rcParams['xtick.major.width'] = 1
    plt.rcParams['xtick.minor.width'] = 1
    plt.rcParams['ytick.major.size'] = 3
    plt.rcParams['ytick.minor.size'] = 3
    plt.rcParams['ytick.major.width'] = 1
    plt.rcParams['ytick.minor.width'] = 1
    plt.rcParams['legend.frameon'] = True
    plt.rcParams['legend.loc'] = 'upper right'
    plt.rcParams['axes.linewidth'] = 1
    plt.rcParams['lines.markersize'] = 5

#============================================================
# TIME DECORATOR

def timer(func):
    """Get runtime of decorated function"""
    def wrapper(*args, **kwargs):
        tIni = time()
        result = func(*args, **kwargs)
        tEnd = time()
        print(f"Fonction {func.__name__} exécutée en {tEnd-tIni:.4f}s")
        return result
    return wrapper

#============================================================
# DATA CLASS

class DataScurve():
    def __init__(self, constantes, optical_depth=[1.], 
                       filename={"epais":     "output/scurve/epais.out",
                                 "mince":     "output/scurve/mince.out",
                                 "critiques": "output/scurve/coord_turning_points.out"}):
        self.constantes    = constantes
        self.optical_depth = optical_depth

        self.epais    = pd.read_csv(filename["epais"],     header=None, delim_whitespace=True, names=['TEMP', 'SIGMA', 'RADIUS', 'X_AD'])
        self.mince    = pd.read_csv(filename["mince"],     header=None, delim_whitespace=True, names=['TEMP', 'SIGMA', 'RADIUS', 'X_AD'])
        self.critique = pd.read_csv(filename["critiques"], header=None, delim_whitespace=True, names=['TEMP', 'SIGMA', 'RADIUS', 'X_AD'])

    def get(self, radius, radius_label, isFrom="epais"):
        if   isFrom == "epais":
            data = self.epais
        elif isFrom == "mince":
            data = self.mince
        elif isFrom == "critique":
            data = self.critique

        closest_value = min(data[radius_label].unique(), key=lambda x:abs(x - radius * self.constantes["R_S"]))
        scurve = data[data[radius_label] == closest_value]

        temp  = scurve["TEMP"].to_numpy()
        sigma = scurve["SIGMA"].to_numpy()
        return temp, sigma
    
    def plot(self, ax, radius, radius_label, plot_epais=True, plot_mince=True, plot_critique=True, plot_optical=True):
        """ Plot S-curve for each branch
        """
        if plot_epais:
            temp, sigma = self.get(radius, radius_label, isFrom="epais")
            line_epais, = ax.plot(sigma, temp, '--', color="orange", label="Branche épaisse")
        else:
            line_epais = None

        if plot_mince:
            temp, sigma = self.get(radius, radius_label, isFrom="mince")
            line_mince, = ax.plot(sigma, temp, '--', color="green", label="Branche mince")
        else:
            line_mince = None

        if plot_critique:
            temp, sigma = self.get(radius, radius_label, isFrom="critique")
            line_crit, = ax.plot(sigma, temp, 'x', color="black", label="Point critique")
        else:
            line_crit = None
        
        if plot_optical and self.optical_depth:
            line_levels = tuple()
            for level in self.optical_depth:
                temp, sigma = self.compute_optical_depth(radius, level)
                line, = ax.plot(sigma, temp, '-.', alpha=0.8, lw=0.8, color='black', label=r"$\tau_{eff} = $" + str(level)) 
                line_levels += (line,)
        else:
            line_levels = None

        return line_epais, line_mince, line_crit, *line_levels

    def compute_optical_depth(self, radius, tau_eff_level, eps=0.01):
        """ Computing levels of \tau_{eff} with bisection method with a given range
        """
        # Temp & Sigma array
        sigma = np.linspace(500, 20000, 100)
        temp  = np.zeros(len(sigma))

        # Function \tau_eff - level ==> find root
        def tau_eff(temp, sigma, radius, level):
            # Constants
            R = (1.380649e-23) / (1.67262192369e-27)
            omega = np.sqrt(6.6743e-11 * self.constantes["MASS"] / (radius * self.constantes["R_S"])**3)

            # Coefficients for H
            a = 0.5 * omega**2 * sigma
            b = -1/3 * 7.56573085e-16 * temp**4
            c = - R * temp * sigma / (2 * self.constantes["MU"])

            # Equations
            H       = (-b + np.sqrt(b**2 - 4 * a * c)) / (2 * a)
            rho     = sigma / (2 * H)
            K_ff    = 6.13e18 * rho * temp**(-3.5)
            tau_eff = 0.5 * sigma * np.sqrt(self.constantes["KAPPA_E"] * K_ff)

            return tau_eff - level
        
        # Bisection method
        for ii, sig in enumerate(sigma):
            # Initial temperatures A & B
            T_A = 1e6
            T_B = 7e7

            # f(A), f(B), f(C)
            y_A = tau_eff(T_A, sig, radius, tau_eff_level)
            y_B = tau_eff(T_B, sig, radius, tau_eff_level) 
            y_C = 1

            while abs(y_C) > eps:
                T_C = (T_A + T_B) / 2
                y_C = tau_eff(T_C, sig, radius, tau_eff_level)

                # No solution
                if y_A * y_B > 0:
                    T_C = 0
                    y_C = 0
                
                elif y_A * y_C > 0: T_A = T_C   # Solution between C and B
                else:               T_B = T_C   # Solution between C and A
            
            temp[ii] = T_C
        
        return temp, sigma

class SkipWrapper(io.TextIOWrapper):
    """ io.TextIOWrapper pour skip les premières lignes des fichiers .out jusqu'à match:
        ## OUTPUT
        ## 
    """
    def __init__(self, file):
        super().__init__(file, line_buffering=True)
        self.f = file
        self.has_matched   = False
        self.has_constants = False
        self.constantes       = {}
        self.constantes_label = {}

    def read(self, size=None):
        while not self.has_matched:
            line = self.readline()
            if "OUTPUT" in line:
                self.has_matched = True
                self.readline()
            
            if "CONSTANTES DE SIMULATION" in line:
                self.has_constants = True
            
            if self.has_constants and "=" in line:
                line = ' '.join(line.split()).split()
                self.constantes[line[-3]] = float(line[-1])
                self.constantes_label[line[-3]] = ' '.join(line[:-3])
        
        return super().read(size)

class DataHandler():
    @timer
    def __init__(self, filename):
        """
            Extract data from an output file from file : [filename]
            -----------
            Attributes:
            .path  : <str> path of the output file
            .df    : <pandas.Dataframe>
            .var   : <list> list of variable names (including space variable at index 0)
            .time  : <numpy.array> of time values
            .space : <numpy.array> of space values

            .time_label  : name of the time variable
            .space_label : name of the space variable

            .scurve : scurve data
        """
        # Path
        self.path = "output/" + filename

        # Constantes initiales
        self.constantes       = {}
        self.constantes_label = {}

        # Data
        self.df     = None
        self.scurve = None
        self.time   = None
        self.space  = None
        self.general_metrics = {}

        # Labels
        self.time_label  = ""
        self.space_label = ""
        self.var         = []

        # Variables LaTeX and units
        self.LaTeX = {
            "MASS"    : (r"$Mass$",             r"$M_\odot$"),
            "M_0_DOT" : (r"$\dot{M_0}$",        r"$M_\odot \cdot yr^{-1}$"),
            "T"       : (r"$t$",                r"$s$"), 
            "RADIUS"  : (r"$r$",                r"$R_s$"),
            "OMEGA"   : (r"$\Omega$",           r"$s^{-1}$"),
            "P"       : (r"$P$",                r"$Pa$"),
            "BETA"    : (r"$\beta$",            r""),
            "C_S"     : (r"$c_s$",              r"$m \cdot s^{-1}$"),
            "H"       : (r"$H$",                r"$m$"),
            "RHO"     : (r"$\rho$",             r"$kg \cdot m^{-3}$"),
            "NU"      : (r"$\nu$",              r"$m^{2} \cdot s^{-1}$"),
            "SIGMA"   : (r"$\Sigma$",           r"$kg \cdot m^{-2}$"),
            "SPEED"   : (r"$v$",                r"$m \cdot s^{-1}$"),
            "TEMP"    : (r"$T$",                r"$K$"),
            "M_DOT"   : (r"$\dot{M}$",          r"$\dot{M_0}$"),
            "F_Z"     : (r"$F_z$",              r"$W \cdot m^{-2}$"),
            "P_GAZ"   : (r"$P_{gaz}$",          r"$Pa$"),
            "P_RAD"   : (r"$P_{rad}$",          r"$Pa$"),
            "Q_PLUS"  : (r"$Q_{+}$",            r"$m^{2} \cdot s^{-3}$"),
            "Q_ADV"   : (r"$Q_{adv}$",          r"$m^{2} \cdot s^{-3}$"),
            "Q_MOINS" : (r"$Q_{-}$",            r"$m^{2} \cdot s^{-3}$"),
            "TAU_EFF" : (r"$\tau_{eff}$",       r""),
            "K_FF"    : (r"$\kappa_{ff}$",      r"$m^{2} \cdot kg^{-1}$"),
            "E_FF"    : (r"$\epsilon_{ff}$",    r"$$"),
            "C_V"     : (r"$C_v$",              r"$m^{2} \cdot s^{2} K^{-1}$"),
            "L_STEFAN": (r"$L$",                r"$L_{Edd}$"),
            }

        # Read output file
        self.read_data()
        
        # Change units
        self.normalize_data(self.space_label, self.constantes["R_S"])
        if "M_DOT" in self.var:                 self.normalize_data("M_DOT", self.constantes["M_0_DOT"])
        if "L_STEFAN" in self.general_metrics:  self.normalize_data("L_STEFAN", self.constantes["L_EDD"])

    def read_data(self):
        """ Read data for initialization
        """
        # Find amount of variables and time array
        with open(self.path, 'rb') as file:
            with SkipWrapper(file) as datafile:
                data = pd.read_csv(datafile, header=None)
                length = len(data[0].iat[1].split())
                self.constantes       = datafile.constantes
                self.constantes_label = datafile.constantes_label

        with open(self.path, 'rb') as file:
            with SkipWrapper(file) as datafile:
                data = pd.read_csv(datafile, header=None, delim_whitespace=True, names=range(length))
        
        # Time array
        self.time_label = data[0].iat[0]
        data_time  = data.loc[data[0] == self.time_label][1]
        self.time  = data_time.to_numpy().astype(np.float64)
        data.drop(data_time.index, axis=0, inplace=True)

        # Space array
        self.space_label = data[0].iat[0]
        data_space       = data.loc[data[0] == self.space_label]
        self.space       = data.iloc[0][1:].to_numpy().astype(np.float64)
        data.drop(data_space.index, axis=0, inplace=True)

        # Multi-index
        self.var = list(data[0].unique())
        data.columns = ["variables"] + list(self.space)
        data[self.time_label] = np.repeat(self.time, len(self.var))
        data.set_index(["T", "variables"], inplace=True)
        data = data.stack()
        data.index.set_names(self.space_label, level=2, inplace=True)

        # Dataframe
        data = data.to_frame()
        data.rename(columns={0: "value"}, inplace=True)

        # Remove time-only data
        for var in self.var:
            datavar = data.xs(var, level="variables")
            if datavar.shape[0] == self.time.shape[0]:
                self.general_metrics[var] = datavar.to_numpy().flatten()

        for var in self.general_metrics:
            self.var.remove(var)
            data.drop(var, level="variables", inplace=True)

        self.df = data

        # Scurve data
        self.scurve = DataScurve(self.constantes)

    def normalize_data(self, variable, norm):
        # Space
        if variable == self.space_label:
            self.space /= norm
        
        # Time
        elif variable == self.time_label:
            self.time /= norm
        
        # F(t) (general metrics)
        elif variable in self.general_metrics:
            self.general_metrics[variable] /= norm
        
        # F(r,t)
        else:
            array = self.df.xs(variable, level="variables")
            self.df[self.df.index.get_level_values("variables") == variable] /= norm

    def get(self, variable, space_idx=None, time_idx=None):
        if variable in self.var:
            array = self.df.query(f"variables=='{variable}'").to_numpy().astype(np.float64)
            array = np.reshape(array, (self.time.shape[0], self.space.shape[0]))

            if time_idx is None and space_idx is None:
                return array

            if time_idx is None:
                return array[:,space_idx]

            if space_idx is None:
                return array[time_idx,:]

            return array[time_idx, space_idx]
        
        elif variable in self.general_metrics:
            return self.general_metrics[variable]

    def plot(self, radius=None, time_min=None, time_max=None, **fig_kw):
        """ Plot toutes les données.
            Si radius est indiqué, on limite les données au rayon considéré en fixant le rayon le plus proche de [radius]
        """
        if radius:
            closest_radius = min(np.unique(self.space), key=lambda x:abs(x-radius))
            self.space = np.array([closest_radius])
            self.df = self.df.loc[:,:,closest_radius]
            print(f"Données restreintes à {self.space_label} = {closest_radius}")

        if not time_min or time_min <= self.time[0]:  idx_min = 0
        else:                                         idx_min = np.arange(self.time.shape[0])[self.time > time_min][0]
        if not time_max or time_max >= self.time[-1]: idx_max = -1
        else:                                         idx_max = np.arange(self.time.shape[0])[self.time < time_max][-1]
        
        if time_min or time_max:
            self.time = self.time[idx_min:idx_max]
            self.df = self.df.loc[self.time[0]:self.time[-1],:,:]
            print(f"Données restreintes à [{self.time[0]} ; {self.time[-1]}]")
        
        # Start plot
        GUI(self, **fig_kw).start()

#============================================================
# PLOTS CLASS

class Plot():
    def __init__(self, data: DataHandler, **fig_kw):
        # Data
        self.data = data
        self.x = None
        self.y = None
        self.time_idx  = None
        self.space_idx = None

        # Figure & axes
        self.figure = Figure(**fig_kw)
        self.ax     = self.figure.add_subplot()
        line,       = self.ax.plot([], [], '.-')
        self.line   = line
        self.optional_lines = []
        self.saved_lines = []
        self.annotation = []
        self.legend = None

        # Plot options
        self.hasGrid = True
        self.xlabel = ""
        self.ylabel = ""
        self.title  = ""

        # Animation options
        self.animation = None
        self.isPaused  = False
        self.animationLine = None
        self.frameStep = int(max(self.data.time.shape[0] / 500, 1))
    
    def relim(self, margin=0.05):
        """ Set the limits of the plot (xlim, ylim) with a given [margin]
        """
        # Get data array at all time or space index
        if self.xlabel in [self.data.time_label, self.data.space_label]:
            x_array = self.x
        else:
            x_array = self.data.get(self.xlabel)
        
        y_array = self.data.get(self.ylabel)

        # Limits
        xlims = [np.nanmin(x_array), np.nanmax(x_array)]
        ylims = [np.nanmin(y_array), np.nanmax(y_array)]

        # Margins
        xmargin = (xlims[1] - xlims[0]) * margin
        ymargin = (ylims[1] - ylims[0]) * margin

        # Set limits
        self.ax.set_xlim(xlims[0] - xmargin, xlims[1] + xmargin)
        self.ax.set_ylim(ylims[0] - ymargin, ylims[1] + ymargin)
    
    def set_axis(self, xlabel=None, ylabel=None):
        """ Set the X-axis and Y-axis of the plot
        """
        # No change
        if self.xlabel == xlabel and self.ylabel == ylabel:
            return

        # Remove saved lines
        for line in self.saved_lines:
            line["line"].remove()
        self.saved_lines = []

        # Set labels
        if xlabel:
            self.xlabel = xlabel
            if self.data.LaTeX[xlabel][1]:  self.ax.set_xlabel(f"{self.data.LaTeX[xlabel][0]} [{self.data.LaTeX[xlabel][1]}]")
            else:                           self.ax.set_xlabel(f"{self.data.LaTeX[xlabel][0]}")
        
        if ylabel:
            self.ylabel = ylabel
            if self.data.LaTeX[ylabel][1]:  self.ax.set_ylabel(f"{self.data.LaTeX[ylabel][0]} [{self.data.LaTeX[ylabel][1]}]")
            else:                           self.ax.set_ylabel(f"{self.data.LaTeX[ylabel][0]}")
        
        # Reset sliders
        self.reset_slider_idx()

        # Set title
        self.set_title()

        # Set new data
        if self.xlabel == self.data.space_label:    self.x = self.data.space
        elif self.xlabel == self.data.time_label:   self.x = self.data.time
        else:                                       self.x = self.data.get(self.xlabel)

        if self.ylabel in self.data.general_metrics.keys(): self.y = self.data.general_metrics[self.ylabel]
        else:                                               self.y = self.data.get(self.ylabel)

        # Redefine limits
        self.relim()

    def set_title(self, title=None):
        """ Set the title to the plot
        """
        if title:
            self.title = title
        else:
            self.title = f"{self.data.LaTeX[self.ylabel][0]}({self.data.LaTeX[self.xlabel][0]})"
        self.ax.set_title(self.title)

    def set_data(self, idx_time=None, idx_space=None):
        """ Set the data to plot: Y(X)
        """
        if self.ylabel in self.data.general_metrics.keys():
            self.line.set_data(self.x, self.y)
            return
            
        # Y(r)
        if self.time_idx is not None:
            self.line.set_data(self.x, self.y[self.time_idx, :])
        
        # Y(t):
        elif self.xlabel == self.data.time_label:
            self.line.set_data(self.x, self.y[:, self.space_idx])
        
        # Y(X)
        else:
            self.line.set_data(self.x[:, self.space_idx], self.y[:, self.space_idx])

    def set_idx(self, space_idx=None, time_idx=None):
        """ Set values of the Slider index
        """
        self.time_idx  = time_idx
        self.space_idx = space_idx

    def reset_slider_idx(self):
        """ Reset to 0 the current Slider index and set to None the other one
        """
        if self.ylabel in self.data.general_metrics:
            return
        # Time Slider
        if self.xlabel == self.data.space_label:
            self.set_idx(time_idx=0)
        
        # Space Slider
        elif self.space_idx is None:
            self.set_idx(space_idx=0)

    def set_annotation(self):
        """ Add text to show radius or time
            Add text to show Mass and M_0_DOT
        """
        # Erase previous artist
        for el in self.annotation:
            el.remove()
        self.annotation = []

        # Radius / Time text
        if self.time_idx is not None:       text = f"$t = {self.data.time[self.time_idx]:.4f}\, s$"         # Time
        elif self.space_idx is not None:    text = f"$r = {self.data.space[self.space_idx]:.2f} \,R_s$"     # Space
        else:                               text = None                                                     # Other

        if text and not self.saved_lines:
            rightNote = self.ax.annotate(
                text, (0.99,1.015), 
                xycoords='axes fraction', 
                verticalalignment='baseline', horizontalalignment='right',
                fontsize=10)
            self.annotation.append(rightNote)

        # Annotations saved lines
        if self.saved_lines:
            notesOffset = (0, (self.ax.get_ylim()[1] - self.ax.get_ylim()[0]) * 0.02)
            notesKwargs = {
                "verticalalignment": "baseline", 
                "horizontalalignment": "right",
                "fontsize": 12
                }
            for el in self.saved_lines:
                x = el["line"].get_xdata()
                y = el["line"].get_ydata()
                note = self.ax.annotate(
                    el["note"], 
                    (x[-1] + notesOffset[0], y[-1] + notesOffset[1]), **notesKwargs)
                self.annotation.append(note)
            
            if self.space_idx is not None:  textCurrentLine = f"${self.data.space[self.space_idx]:.2f} \,R_s$"
            else:                           textCurrentLine = f"${self.data.time[self.time_idx]:.4f}\, s$"

            x = self.line.get_xdata()
            y = self.line.get_ydata()
            noteCurrentLine = self.ax.annotate(
                textCurrentLine,
                (x[-1] + notesOffset[0], y[-1] + notesOffset[1]), **notesKwargs)
            self.annotation.append(noteCurrentLine)

        # Mass & M_0_dot
        massValue = self.data.constantes["MASS"] / 1.989e30                      # in Solar Mass
        mdotValue = self.data.constantes["M_0_DOT"] / 1.989e30 * 86400 * 365.25  # in Solar Mass / Myr
        leftNote = self.ax.annotate(
            f"{self.data.LaTeX['MASS'][0]} = {massValue:.2f} {self.data.LaTeX['MASS'][1]}"  \
            + "\n"                                                                          \
            + f"{self.data.LaTeX['M_0_DOT'][0]} = {mdotValue:.2e} {self.data.LaTeX['M_0_DOT'][1]}",
            (0.05, 1.015),
            xycoords='axes fraction',
            verticalalignment='baseline', horizontalalignment='left',
            fontsize=10)
        self.annotation.append(leftNote)

    def update(self):
        """ Update plot
        """
        # Plot
        self.set_annotation()
        self.set_data()

        # Grid
        if self.hasGrid:    self.ax.grid(True)
        else:               self.ax.grid(False)

        # Destroy optional lines
        for line in self.optional_lines:
            if line:
                self.ax.lines.remove(line)
        self.optional_lines = []

        # Legend
        self.legend = self.ax.get_legend()

        # S-curve => TEMP(SIGMA)
        if self.ylabel == "TEMP" and self.xlabel == "SIGMA":
            self.plotScurve()
            self.ax.legend(title=r"Courbe en S ($Q_{+} - Q_{-} = 0$)")
        elif self.ylabel in ["TEMP", "SIGMA"] and self.xlabel == self.data.space_label:
            self.plotCritique()
            if self.optional_lines:
                self.ax.legend()
        elif self.legend:
            self.legend.remove()

    def save_data(self):
        """ Save current self.line for this Y(X) sketch and add legend
        """
        # Y(t)
        if self.space_idx is not None:
            note = f"${self.data.space[self.space_idx]:.2f} \,R_s$"
        
        # Y(x)
        elif self.time_idx is not None:
            note = f"${self.data.time[self.time_idx]:.4f}\, s$"
        
        # L_Stefan
        else:
            return

        x = self.line.get_xdata()
        y = self.line.get_ydata()
        line, = self.ax.plot(x, y, '.-')
        self.saved_lines += [{"line": line, "note": note}]

    def plotCritique(self):
        """ Add axhline & axvline for critical values of Temp / Sigma
        """
        # Get critical dataframe
        critique = self.data.scurve.critique

        # F(r) at t
        if self.xlabel == self.data.space_label:
            temp   = critique["TEMP"].to_numpy()
            sigma  = critique["SIGMA"].to_numpy()
            radius = critique[self.data.space_label].to_numpy() / self.data.constantes["R_S"]
            if self.ylabel == "TEMP":
                line,  = self.ax.plot(radius, temp, linestyle='-.', lw=0.8, alpha=0.8, color='black', label=f'{self.data.LaTeX["TEMP"][0]} critique')
            elif self.ylabel == "SIGMA":
                line,  = self.ax.plot(radius, sigma, linestyle='-.', lw=0.8, alpha=0.8, color='black', label=f'{self.data.LaTeX["SIGMA"][0]} critique')
            else:
                return
            
        self.optional_lines += [line]

    def plotScurve(self):
        """ Add S-Curve lines and Optical Depth line
        """
        # S-Curve
        lines = self.data.scurve.plot(self.ax, self.data.space[self.space_idx], self.data.space_label)
        self.optional_lines += list(lines)

    def start_animation(self, slider_to_update=None):
        """ Animation func
        """
        def updateTime(frame_number):
            """ Animation function for Y(r)
            """
            self.time_idx = (self.time_idx + self.frameStep) % self.data.time.shape[0]
            self.set_annotation()
            self.set_data()
            if slider_to_update:
                slider_to_update.set(self.time_idx)
        
        def updateSpace(frame_number):
            """ Animation function for Y(X) with X /= r
            """
            idx = frame_number % self.data.time.shape[0]
            if self.ylabel in self.data.general_metrics.keys():
                self.animationLine.set_data(self.x[idx], self.y[idx])
            elif self.xlabel == self.data.time_label:
                self.animationLine.set_data(self.x[idx], self.y[idx, self.space_idx])
            else:
                self.animationLine.set_data(self.x[idx, self.space_idx], self.y[idx, self.space_idx])

        # Whole plot animation
        if self.space_idx is None:
            self.animation = animation.FuncAnimation(self.figure, updateTime, interval=10)

        # Point animation
        if self.time_idx is None:
            line, = self.ax.plot([], [], 'ro')
            self.animationLine = line
            self.animation = animation.FuncAnimation(self.figure, updateSpace, interval=10)
     
    def stop_animation(self):
        if self.animation is not None:
            self.animation._stop()
            self.animation = None
        if self.animationLine:
            self.animationLine.set_data([], [])
            self.animationLine = None
    
    def savefig(self, name):
        self.figure.savefig(f"image/{name}.png", dpi=100)
           
#============================================================
# GUI CLASS
    
class GUI():
    # INIT
    def __init__(self, data: DataHandler, **fig_kw):
        # Pandas dataframe
        self.data = data

        # Windows (and resize)
        self.root = tkinter.Tk()
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(0, weight=1)

        # Toolbar (above)
        self.toolbar = tkinter.Frame(self.root)
        self.toolbar.pack(side=tkinter.TOP, fill=tkinter.X)
        self.toolbar.columnconfigure(1, weight=2)
        self.toolbar.columnconfigure(3, weight=2)

        # Matplotlib embed
        self.plot = Plot(self.data)
        self.canvas = FigureCanvasTkAgg(self.plot.figure, self.root)
        NavigationToolbar2Tk(self.canvas, self.root)
        self.canvas.get_tk_widget().pack(fill=tkinter.BOTH, expand=True)

        # Toolbar > Y-axis
        self.YaxisLabel   = ttk.Label(self.toolbar, text="Y-axis variable").grid(row=0, column=0, sticky=tkinter.E, padx=5, pady=5)
        self.YaxisOptions = tkinter.StringVar(self.toolbar)
        self.YaxisMenu    = ttk.Combobox(
            self.toolbar,
            textvariable=self.YaxisOptions,
            values=tuple(self.data.var) + tuple(self.data.general_metrics.keys()),
            state="readonly"
        )
        self.YaxisMenu.current(0)
        self.YaxisMenu.grid(row=0, column=1, sticky=tkinter.W, padx=5, pady=5)
        self.YaxisMenu.bind("<<ComboboxSelected>>", self.event_yaxis)

        # Toolbar > X-axis
        self.XaxisLabel   = ttk.Label(self.toolbar, text="X-axis variable").grid(row=0, column=2, sticky=tkinter.E, padx=5, pady=5)
        self.XaxisOptions = tkinter.StringVar(self.toolbar)
        self.XaxisMenu    = ttk.Combobox(
            self.toolbar,
            textvariable=self.XaxisOptions,
            values=(self.data.time_label, self.data.space_label,) + tuple(self.data.var),
            state="readonly"
        )
        self.XaxisMenu.current(0)
        self.XaxisMenu.grid(row=0, column=3, sticky=tkinter.W, padx=5, pady=5)
        self.XaxisMenu.bind("<<ComboboxSelected>>", self.event_xaxis)
        if self.data.space.shape[0] <= 1:
            self.XaxisMenu.configure(values=(self.data.time_label,)+tuple(self.data.var))

        # Toolbar > Sliders
        self.TimeFrame  = ttk.Frame(self.toolbar)
        self.TimeFrame.grid(row=1, column=0, columnspan=5, sticky=tkinter.NSEW)
        self.TimeLabel  = ttk.Label(self.TimeFrame, text=f"{self.data.time_label} = {self.data.time[0]:.4f}s")
        self.TimeValue  = tkinter.IntVar(self.TimeFrame)
        self.TimeSlider = ttk.Scale(
            self.TimeFrame,
            from_=0,
            to=self.data.time.shape[0] - 1,
            variable=self.TimeValue,
            command=self.event_timeSlider
        )
        if self.data.time.shape[0] > 1:
            self.TimeLabel.pack(side=tkinter.LEFT, padx=5, pady=5)
            self.TimeSlider.pack(side=tkinter.LEFT, padx=20, pady=5, expand=True, fill=tkinter.X)

        self.SpaceFrame  = ttk.Frame(self.toolbar)
        self.SpaceFrame.grid(row=1, column=0, columnspan=5, sticky=tkinter.NSEW)
        self.SpaceLabel  = ttk.Label(self.SpaceFrame, text=f"{self.data.space_label} = {self.data.space[0]:.2f} Rs")
        self.SpaceValue  = tkinter.IntVar(self.SpaceFrame)
        self.SpaceSlider = ttk.Scale(
            self.SpaceFrame,
            from_=0,
            to=self.data.space.shape[0] - 1,
            variable=self.SpaceValue,
            command=self.event_spaceSlider
        )
        self.SpaceTextValue = tkinter.StringVar(value=self.data.space[0])

        if self.data.space.shape[0] > 1:
            self.SpaceLabel.pack(side=tkinter.LEFT, padx=5, pady=5)
            self.SpaceSlider.pack(side=tkinter.LEFT, padx=20, pady=5, expand=True, fill=tkinter.X)

        # Toolbar > Animation button
        self.animationButton = ttk.Button(
            self.toolbar,
            text="Start",
            command=self.event_animation
        )
        self.animationButton.grid(row=0, column=5, sticky=tkinter.W, padx=20, pady=5)

        # Toolbar > Savefig button
        self.savefigButton = ttk.Button(
            self.toolbar, 
            text='Savefig',
            command = self.event_savefig
        )
        self.savefigButton.grid(row=1, column=6, sticky=tkinter.W, padx=20, pady=5)

        # Toolbar > Add plot
        self.addplotButton = ttk.Button(
            self.toolbar,
            text="Add plot",
            command=self.event_addplot
        )
        self.addplotButton.grid(row=0, column=6, sticky=tkinter.W, padx=20, pady=5)
        
        # Configure style
        self.style = ttk.Style(self.root)
        # self.style.configure('TFrame', )
        try:
            self.style.theme_use('alt')
        except:
            pass

    def event_addplot(self):
        """ Add a new line to the plot and save the current one
        """
        self.addplotButton.selection_clear()
        self.plot.save_data()

    def event_savefig(self):
        """ Take a screenshot of the current plot
        """
        self.savefigButton.selection_clear()

        value_space = f"{self.data.space[self.SpaceValue.get()]:.2f}"
        value_time  = f"{self.data.time[self.TimeValue.get()]:.2f}"
        mdot        = self.data.constantes['M_0_DOT']
        
        if self.plot.xlabel == self.data.space_label and self.plot.ylabel != 'L_STEFAN':
            name = f"{self.XaxisOptions.get()}_{self.YaxisOptions.get()}_{value_time}s_ALL_R"   #+'_'+mdot
        elif self.plot.ylabel == 'L_STEFAN':
            name = f"{self.XaxisOptions.get()}_{self.YaxisOptions.get()}"
        else:
            name = f"{self.XaxisOptions.get()}_{self.YaxisOptions.get()}_{value_space}Rs_ALL_T" #mdot
     
        if self.plot.animation:
            if self.plot.isPaused:
                self.plot.savefig(name)
            else :
                self.plot.animation.pause()
                self.plot.savefig(name)
                self.plot.animation.resume()
        else :
            self.plot.savefig(name)

    def event_xaxis(self, event):
        """ Change in X-axis event
        """
        # Clear Menu
        self.XaxisMenu.selection_clear()

        # Stop animation
        self.plot.stop_animation()
        self.animationButton.configure(text="Start")

        # Change axis
        previous_xlabel = self.plot.xlabel
        self.plot.set_axis(xlabel=self.XaxisOptions.get())

        # Switch between sliders
        if self.plot.xlabel == self.data.space_label:
            self.TimeFrame.tkraise()
        else:
            self.SpaceFrame.tkraise()
        
        # Reset Time or Space sliders
        if self.plot.xlabel == self.data.space_label:
            self.TimeSlider.set(0)
            self.TimeSlider.update()
        elif previous_xlabel == self.data.space_label:
            self.SpaceSlider.set(0)
            self.SpaceSlider.update()
        else:
        # Update plot
            self.update()

    def event_yaxis(self, event):
        """ Change in Y-axis event
        """
        # Clear Menu
        self.YaxisMenu.selection_clear()

        # Stop animation
        self.plot.stop_animation()
        self.animationButton.configure(text="Start")

        # Change axis
        self.plot.set_axis(ylabel=self.YaxisOptions.get())

        # Remove sliders and desactivate X-axis if Y-axis is F(t) (instead of F(t,r))
        if self.plot.ylabel in self.data.general_metrics:
            self.SpaceFrame.tkraise()
            self.TimeFrame.grid_remove()
            self.SpaceFrame.grid_remove()
            self.XaxisMenu.configure(state="disabled")
            self.XaxisMenu.set(self.data.time_label)
            self.plot.set_idx()
            self.plot.set_axis(xlabel=self.XaxisOptions.get())
        else:
            self.TimeFrame.grid()
            self.SpaceFrame.grid()
            self.XaxisMenu.configure(state="readonly")

        # Reset Time slider
        if self.plot.xlabel == self.data.space_label:
            self.TimeSlider.set(0)
            self.TimeSlider.update()
        else:
        # Update plot
            self.update()

    def event_timeSlider(self, event):
        """ Change in Slider value (Time)
        """
        # Change index value
        value = self.TimeValue.get()
        self.TimeLabel.config(text=f"{self.data.time_label} = {self.data.time[value]:.4f}s")

        if not self.plot.animation or self.plot.isPaused:
            self.plot.set_idx(time_idx=value)

            # Update plot
            self.update()

    def event_spaceSlider(self, event):
        """ Change in Slider value (Space)
        """
        # Stop animation
        self.plot.stop_animation()
        self.animationButton.configure(text="Start")

        # Change index value
        value = self.SpaceValue.get()
        self.SpaceLabel.config(text=f"{self.data.space_label} = {self.data.space[value]:.2f} Rs")
        self.plot.set_idx(space_idx=value)

        # Update plot
        self.update()
    
    def event_animation(self):
        """ Start / Stop plot animation
        """
        # Clear button
        self.animationButton.selection_clear()

        # Play/Pause animation
        if self.plot.animation:
            if self.plot.isPaused:
                self.plot.animation.resume()
                self.animationButton.configure(text="Pause")
            else:
                self.plot.animation.pause()
                self.animationButton.configure(text="Play")
            
            self.plot.isPaused = not self.plot.isPaused
        
        # Start animation
        else:
            self.plot.isPaused = False
            self.animationButton.configure(text="Pause")
            self.plot.start_animation(slider_to_update=self.TimeSlider)
            self.canvas.draw()

    def update(self):
        """ Mise à jour du plot """
        self.plot.update()
        self.canvas.draw()

    def start(self):
        self.plot.set_axis(xlabel=self.XaxisOptions.get(), ylabel=self.YaxisOptions.get())
        self.update()
        tkinter.mainloop()

#============================================================
#============================================================
#                       MAIN PROGRAM                        #
#============================================================
#============================================================
"""
    data.time  => np.array 1D : valeurs temporelles
    data.space => np.array 1D: valeurs spatiales
    data.var   => liste de labels des variables utilisées:
        0 : spatiale (X_AD, RADIUS)
        ..: OMEGA, H, TEMP, etc.
    
    data.get(variable, space_idx=None, time_idx=None)
        ==> np.array correspondant à "variable" pour l'indice spatial "space_idx" et temporel "time_idx"
            Si les indices sont None, renvoie le vecteur correspondant
        
        Ex:
        data.get("OMEGA")               ==> np.array 2D de Omega à toutes les positions et tous les instants
        data.get("SIGMA", space_idx=0)  ==> np.array 1D de Sigma à la position data.space[0] et à tous les instants
        data.get("C_S_AD", time_idx=30) ==> np.array 1D de c_s_ad au temps t = data.time[30] et à toutes les positions
        
        data.get("M_DOT", space_idx=-1, time_idx=-1) ==> np.array de 1 élément correspondant à M_dot au bord extérieur et au temps final
"""
"""
    data.plot() => lance matplotlib
    options:
    - radius=xxx   : limiter les données au rayon indiqué (pas besoin d'être précis sur la valeur tant qu'on est proche)
    - time_min=xxx : on élimine les données telles qu'on n'aura plus que TIME > time_min
    - time_max=xxx : on élimine les données telles qu'on n'aura plus que TIME > time_max
"""

if __name__ == "__main__":
    # Parser
    parser = argparse.ArgumentParser(
        description="Affiche les données obtenues par la simulation.",
        allow_abbrev=True)
    parser.add_argument("output",
                         help="Nom du fichier de sortie (Ex: [data.out])")

    args = parser.parse_args()

    # Plot data
    data = DataHandler(args.output)
    init_plotting()
    data.plot()