import numpy as np
from matplotlib.figure import Figure
from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import (FigureCanvasTkAgg, NavigationToolbar2Tk)
import matplotlib.backends.backend_tkagg as tkagg

import tkinter
from tkinter import ttk
import pandas as pd
from xarray import Dataset


class DataHandler():
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
        """
        # Path
        self.path = "output/" + filename

        # Dataframe
        data = pd.read_csv(self.path, header=None, on_bad_lines='skip', dtype="string")

        # Skip the first rows of the dataframe
        start_index = 0
        for index, row in data.iterrows():
            if "OUTPUT" in row[0]:
                start_index = index+2
                break
        data = data.iloc[start_index:, :]
        data = data.reset_index(drop=True)
        data[0] = data[0].apply(lambda x: x.split())

        # Numpy array of the time
        time_array = data[data[0].apply(lambda x: len(x)) == 2][0].apply(pd.Series).rename(columns={1:"Time"}).drop(0, axis=1).reset_index(drop=True).to_numpy()

        # Other variables
        data      = data[data[0].apply(lambda x: len(x)) != 2][0].apply(pd.Series).reset_index(drop=True)
        name_var  = data[0].unique()

        # Numpy array of space
        space_array = data.iloc[0].drop(0).to_frame(name=name_var[0]).reset_index(drop=True).to_numpy()

        # Numpy array of every other variables
        var_array = {}
        for name in name_var[1:]:
            var_array[name] = data[data[0] == name].drop(0, axis=1).to_numpy()
        
        # Data qui contient la valeur pour chaque variable dans un tableau de dimension (TEMPS * SPACE * NB_VARIABLES)
        data_array = np.zeros((time_array.shape[0], space_array.shape[0], len(name_var[1::])))
        for ii, (_, array) in enumerate(var_array.items()):
            data_array[:,:,ii] = array
        
        data_vars = dict()
        coords = ["temps", "variables"]
        for ii, dx in enumerate(space_array.flatten()):
            dict_value = dict(((dx, (coords, data_array[:,ii,:])),))
            data_vars.update(dict_value)

        # Dataset => dataframe
        dataset = Dataset(
            data_vars=data_vars,
            coords={
                "temps": time_array[:,0],
                "variables": name_var[1:]
            },
        )

        self.df    = dataset.to_dataframe()
        self.var   = name_var
        self.time  = time_array.astype(float).flatten()
        self.space = space_array.astype(float).flatten()

    def get(self, variable, space_idx=None, time_idx=None):
        array = self.df.query(f"variables=='{variable}'").to_numpy()

        if time_idx is None and space_idx is None:
            return array

        if time_idx is None:
            return array[:,space_idx]

        if space_idx is None:
            return array[time_idx,:]

        return array[time_idx, space_idx]

    def plot(self, **fig_kw):
        GUI = FigureGUI(self, figsize=(10,4), dpi=100, **fig_kw)
        GUI.start()

class FigureGUI():
    # INIT
    def __init__(self, data, title="", hasToolbar=True, **fig_kw):
        # Pandas dataframe
        self.data   = data

        # Windows (and resize)
        self.root = tkinter.Tk()
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(0, weight=1)

        # Title
        self.title  = None
        if title:
            self.title = ttk.Label(self.root, text=title)
            self.title.grid(row=0, column=0, padx=10, pady=10)
        
        # Matplotlib embed
        self.figure = Figure(**fig_kw)
        self.ax     = self.figure.add_subplot()
        line,       = self.ax.plot([], [])
        self.line   = line

        self.canvas = FigureCanvasTkAgg(self.figure, master=self.root)
        self.canvas.draw()
        self.canvas.get_tk_widget().grid(row=1, column=0, ipadx=40, ipady=20)
        
        # Toolbar enhanced
        self.toolbar = None
        if hasToolbar:
            self.footer = ttk.Frame(master=self.root)
            self.footer.grid(row=2, column=0)

            # Matplotlib default toolbar
            toolbarFrame = tkinter.Frame(master=self.footer)
            toolbarFrame.grid(row=0,column=0, sticky=tkinter.W)
            self.toolbar = NavigationToolbar2Tk(self.canvas, toolbarFrame)
            self.toolbar.update()

            # Combobox to select variables
            self.optionsList  = tkinter.StringVar(self.footer)
            self.optionsLabel = ttk.Label(self.footer, text="Variables :")
            self.optionsMenu  = ttk.Combobox(
                self.footer,
                textvariable=self.optionsList,
                values=tuple(self.data.var[1:]),
                state="readonly",
                width=20)
            self.optionsMenu.bind("<<ComboboxSelected>>", self.set_variable_event)
            self.optionsLabel.grid(column=1, row=0, sticky=tkinter.W, padx=5, pady=5)
            self.optionsMenu.grid(column=2, row=0, sticky=tkinter.W, padx=5, pady=5)

            # Checkbox to switch between time and space as x-axis
            self.isTimeBool     = tkinter.BooleanVar(self.footer)
            self.isTimeCheckbox = ttk.Checkbutton(
                self.footer,
                text="Time",
                variable=self.isTimeBool,
                command=self.set_spaceTime
            )
            self.isTimeCheckbox.grid(column=3, row=0, sticky=tkinter.W)

            # Slider to select value
            self.SliderValue = tkinter.IntVar(self.footer)
            self.SliderLabel = ttk.Label(self.footer)
            self.Slider      = ttk.Scale(
                self.footer,
                length=200,
                from_=0,
                variable=self.SliderValue,
                command=self.set_slider_event
            )
            self.SliderValueLabel = ttk.Label(self.footer)

            self.SliderLabel.grid(column=1, row=1)
            self.Slider.grid(column=2, row=1)
            self.SliderValueLabel.grid(column=1, row=2)

    # EVENTS
    def set_variable_event(self, event):
        """ handle the variable change event"""
        self.optionsMenu.selection_clear()
        variable = self.optionsList.get()
        self.set_variable(variable)

    def set_slider_event(self, event):
        """ handle the slider event """
        self.update_plot()

    # PLOT SETTERS
    def set_variable(self, variable):
        """ Set Y-axis variable, ylabel and ylim"""
        self.variable = variable
        self.ax.set_ylabel(variable)
        minmax = [np.nanmin(self.data.get(variable)), np.nanmax(self.data.get(variable))]
        yrange = minmax[1] - minmax[0]
        self.ax.set_ylim(minmax[0] - yrange/20., minmax[1] + yrange/20.)
        self.update_plot()

    def set_spaceTime(self):
        """ Set X-axis variable, xlabel and xlim
            Also switch Slider time / space
        """
        if self.isTimeBool.get():
            # Slider : Space
            self.Slider["to"] = self.data.space.shape[0] - 1
            self.SliderLabel.configure(text=self.data.var[0])
            label  = "Time"
            array  = self.data.time
        else:
            # Slider : Time
            self.Slider["to"] = self.data.time.shape[0] - 1
            self.SliderLabel.configure(text="Time")
            label = self.data.var[0]
            array = self.data.space
        
        self.Slider.set(0)

        minmax = [np.nanmin(array), np.nanmax(array)]
        xrange = minmax[1] - minmax[0]
        self.ax.set_xlabel(label)
        self.ax.set_xlim(minmax[0] - xrange/20., minmax[1] + xrange/20.)
        self.update_plot()

    # START & UPDATE
    def start(self, variable=None):
        if not variable:
            variable = self.data.var[1]
        
        self.optionsMenu.set(variable)
        self.set_variable(variable)
        self.set_spaceTime()

        tkinter.mainloop()

    def update_plot(self, hasGrid=True):
        """ Check all getters and update the canvas"""
        fixedValue = self.SliderValue.get()

        if self.isTimeBool.get():
            x = self.data.time
            y = self.data.get(self.variable, space_idx=fixedValue)
            self.ax.set_title(f"{self.variable} en fonction du temps")
            self.SliderValueLabel.configure(text=f"{self.data.space[fixedValue]:.4f}")
        else:
            x = self.data.space
            y = self.data.get(self.variable, time_idx=fixedValue)
            self.ax.set_title(f"{self.variable} en fonction du rayon")
            self.SliderValueLabel.configure(text=f"{self.data.time[fixedValue]:.4f}")

        self.line.set_data(x, y)

        if hasGrid:
            self.ax.grid(True)

        self.canvas.draw()

# Récupérer le fichier de sortie
# !!! 
data = DataHandler("data_py.out")

# Numpy array :
data.space # array spatial
data.time  # array du temps
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

# START MATPLOTLIB :
data.plot()