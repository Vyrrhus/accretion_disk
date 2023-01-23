import numpy as np
from matplotlib.figure import Figure
from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import (FigureCanvasTkAgg, NavigationToolbar2Tk)

import tkinter
from tkinter import ttk
import pandas as pd

from time import time
import io

#============================================================
# FILENAME
FILENAME = "data.out"

#============================================================
# MATPLOTLIB GLOBAL SETTINGS

def init_plotting():
    plt.rcParams['figure.figsize'] = (8, 5)
    plt.rcParams['font.size'] = 10
    # plt.rcParams['font.family'] = 'Times New Roman'
    plt.rcParams['axes.labelsize'] = plt.rcParams['font.size']
    plt.rcParams['axes.titlesize'] = 1.5*plt.rcParams['font.size']
    plt.rcParams['legend.fontsize'] = plt.rcParams['font.size']
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

    # plt.gca().spines['right'].set_color('none')
    # plt.gca().spines['top'].set_color('none')
    # plt.gca().xaxis.set_ticks_position('bottom')
    # plt.gca().yaxis.set_ticks_position('left')

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
# CLASS

class DataScurve():
    def __init__(self, filename):
        self.data = pd.read_csv(f"output/{filename}", header=None, delim_whitespace=True, names=['T', 'Sigma', 'RADIUS', 'X_AD'])
    
    def get(self, radius, radius_label):
        closest_value = min(self.data[radius_label].unique(), key=lambda x:abs(x-radius))
        scurve = self.data[self.data[radius_label] == closest_value]

        temp  = scurve["T"].to_numpy()
        sigma = scurve["Sigma"].to_numpy()
        return temp, sigma

class SkipWrapper(io.TextIOWrapper):
    """ io.TextIOWrapper pour skip les premières lignes des fichiers .out jusqu'à match:
        ## OUTPUT
        ## 
    """
    def __init__(self, file):
        super().__init__(file, line_buffering=True)
        self.f = file
        self.has_matched = False

    def read(self, size=None):
        while not self.has_matched:
            line = self.readline()
            if "OUTPUT" in line:
                self.has_matched = True
                self.readline()
        
        return super().read(size)

class DataHandler():
    @timer
    def __init__(self, filename, scurve_filename="results_epais.out"):
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

        # Find amount of variables and time array
        with open(self.path, 'rb') as file:
            with SkipWrapper(file) as datafile:
                data = pd.read_csv(datafile, header=None)
                length = len(data[0].iat[1].split())

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
        self.df = data

        # Scurve data
        self.scurve = DataScurve(scurve_filename)

    def get(self, variable, space_idx=None, time_idx=None):
        array = self.df.query(f"variables=='{variable}'").to_numpy().astype(np.float64)
        array = np.reshape(array, (self.time.shape[0], self.space.shape[0]))

        if time_idx is None and space_idx is None:
            return array

        if time_idx is None:
            return array[:,space_idx]

        if space_idx is None:
            return array[time_idx,:]

        return array[time_idx, space_idx]

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
        
        GUI = FigureGUI(self, **fig_kw)
        GUI.start()

class FigureGUI():
    # INIT
    def __init__(self, data: DataHandler, **fig_kw):
        # Pandas dataframe
        self.data = data

        # Plot options (default values)
        self.hasGrid = True
        self.x = self.data.time
        self.y = None
        self.xlabel = self.data.time_label
        self.ylabel = self.data.var[0]
        self.title  = ""
        self.timeIdx  = None
        self.spaceIdx = 0

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
        self.figure = Figure(**fig_kw)
        self.ax     = self.figure.add_subplot()
        line,       = self.ax.plot([], [], '.-')
        self.line   = line

        self.canvas = FigureCanvasTkAgg(self.figure, self.root)
        NavigationToolbar2Tk(self.canvas, self.root)
        self.canvas.get_tk_widget().pack(fill=tkinter.BOTH, expand=True)

        # Toolbar > Y-axis
        self.YaxisLabel   = ttk.Label(self.toolbar, text="Y-axis variable").grid(row=0, column=0, sticky=tkinter.E, padx=5, pady=5)
        self.YaxisOptions = tkinter.StringVar(self.toolbar)
        self.YaxisMenu    = ttk.Combobox(
            self.toolbar,
            textvariable=self.YaxisOptions,
            values=tuple(self.data.var),
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
        self.TimeFrame.grid(row=1, column=0, columnspan=4, sticky=tkinter.NSEW)
        self.TimeLabel  = ttk.Label(self.TimeFrame, text=f"{self.data.time_label} = {self.data.time[0]:.4f}")
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

        print(self.data.space_label, type(self.data.space_label), self.data.space[0], type(self.data.space[0]))
        self.SpaceFrame  = ttk.Frame(self.toolbar)
        self.SpaceFrame.grid(row=1, column=0, columnspan=4, sticky=tkinter.NSEW)
        self.SpaceLabel  = ttk.Label(self.SpaceFrame, text=f"{self.data.space_label} = {self.data.space[0]:.4f}")
        self.SpaceValue  = tkinter.IntVar(self.SpaceFrame)
        self.SpaceSlider = ttk.Scale(
            self.SpaceFrame,
            from_=0,
            to=self.data.space.shape[0] - 1,
            variable=self.SpaceValue,
            command=self.event_spaceSlider
        )
        self.SpaceTextValue = tkinter.StringVar(value=self.data.space[0])
        # self.SpaceSpinbox = ttk.Spinbox(
        #     self.SpaceFrame,
        #     from_=0,
        #     to=self.data.space[-1],
        #     textvariable=self.SpaceTextValue,
        #     wrap=False,
        #     values=tuple(self.data.space)
        # )
        if self.data.space.shape[0] > 1:
            self.SpaceLabel.pack(side=tkinter.LEFT, padx=5, pady=5)
            # self.SpaceSpinbox.pack(side=tkinter.LEFT, padx=5, pady=5)
            self.SpaceSlider.pack(side=tkinter.LEFT, padx=20, pady=5, expand=True, fill=tkinter.X)

        # Configure style
        self.style = ttk.Style(self.root)
        # self.style.configure('TFrame', )
        try:
            self.style.theme_use('alt')
        except:
            pass

    def event_xaxis(self, event):
        self.XaxisMenu.selection_clear()
        previous_xlabel = self.xlabel
        self.xlabel = self.XaxisOptions.get()
        self.ax.set_xlabel(self.xlabel)

        # Switch Slider (spatial => temporel)
        if self.xlabel == self.data.space_label and previous_xlabel != self.data.space_label:
            self.TimeFrame.tkraise()
            self.spaceIdx = None
            self.timeIdx = 0
            self.set_values()
        
        # Switch Slider (temporel => spatial)
        elif self.xlabel != self.data.space_label and previous_xlabel == self.data.space_label:
            self.SpaceFrame.tkraise()
            self.timeIdx = None
            self.spaceIdx = 0
            self.set_values()
        
        # No switch
        else:
            self.set_values(which="X")

        self.relim()
        self.update()

    def event_yaxis(self, event):
        self.YaxisMenu.selection_clear()
        self.ylabel = self.YaxisOptions.get()
        self.ax.set_ylabel(self.ylabel)
        
        self.set_values(which="Y")
        self.relim()
        self.update()

    def event_timeSlider(self, event):
        self.timeIdx = self.TimeValue.get()
        self.TimeLabel.config(text=f"{self.data.time_label} = {self.data.time[self.timeIdx]:.4f}")

        self.set_values()
        self.update()

    def event_spaceSlider(self, event):
        self.spaceIdx = self.SpaceValue.get()
        self.SpaceLabel.config(text=f"{self.data.space_label} = {self.data.space[self.spaceIdx]:.4f}")

        self.set_values()
        self.update()

    def set_values(self, which="BOTH"):
        # Set X value
        if which in ["BOTH", "X"]:
            if self.xlabel == self.data.space_label:  self.x = self.data.space
            elif self.xlabel == self.data.time_label: self.x = self.data.time
            else:                                     self.x = self.data.get(self.xlabel, space_idx=self.spaceIdx, time_idx=self.timeIdx)
        
        # Set Y value
        if which in ["BOTH", "Y"]:
            self.y = self.data.get(self.ylabel, space_idx=self.spaceIdx, time_idx=self.timeIdx)

    def relim(self):
        # Limits
        if self.xlabel == self.data.time_label or self.xlabel == self.data.space_label:
            x_array = self.x
        else:
            x_array = self.data.get(self.xlabel)
        y_array = self.data.get(self.ylabel)

        xlims = [np.nanmin(x_array), np.nanmax(x_array)]
        ylims = [np.nanmin(y_array), np.nanmax(y_array)]
        xmargin = (xlims[1] - xlims[0]) * 0.05
        ymargin = (ylims[1] - ylims[0]) * 0.05
        self.ax.set_xlim(xlims[0] - xmargin, xlims[1] + xmargin)
        self.ax.set_ylim(ylims[0] - ymargin, ylims[1] + ymargin)

    def update(self):
        """ Mise à jour du plot """
        # Data
        self.line.set_data(self.x, self.y)

        # Title
        self.ax.set_title(f"{self.ylabel}({self.xlabel})")

        # Grid
        if self.hasGrid:
            self.ax.grid(True)
        else:
            self.ax.grid(False)

        # Additionnal custom features
        self.custom_plot()

        self.canvas.draw()

    def start(self):
        self.set_values()
        self.relim()
        self.update()
        self.ax.set_xlabel(self.xlabel)
        self.ax.set_ylabel(self.ylabel)
        tkinter.mainloop()

    def custom_plot(self):
        # S-curve
        if self.xlabel == "SIGMA" and self.ylabel == "TEMP":
            radius = self.data.space[self.spaceIdx]
            temp, sigma = self.data.scurve.get(radius, self.data.space_label)
            try:
                self.scurve_line.set_data(sigma*10, temp)
            except:
                line,            = self.ax.plot(sigma*10, temp, '--', label="Courbe en S")
                self.scurve_line = line

            self.ax.legend()

        else:
            try:
                self.scurve_line.set_data([], [])
                _lg = self.ax.get_legend()
                _lg.remove()
                
            except:
                pass

#============================================================
# MAIN PROGRAM

# Récupérer le fichier de sortie
data = DataHandler(FILENAME)

# Numpy array :
data.space # array spatial
data.time  # array du temps

print(type(data.time[0]))

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

# Plot les données
init_plotting()
data.plot()

"""
    data.plot() => lance matplotlib
    options:
    - radius=xxx   : limiter les données au rayon indiqué (pas besoin d'être précis sur la valeur tant qu'on est proche)
    - time_min=xxx : on élimine les données telles qu'on n'aura plus que TIME > time_min
    - time_max=xxx : on élimine les données telles qu'on n'aura plus que TIME > time_max
"""
