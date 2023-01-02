import numpy as np
from matplotlib import pyplot as plt
from matplotlib.backends.backend_tkagg import (FigureCanvasTkAgg, NavigationToolbar2Tk)
import tkinter as tk
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

    def plot(self, _):
        """ On va gérer le GUI de Matplotlib pour afficher tout ce qu'on veut :
            - chaque variable dans l'espace à t fixé
            - chaque variable dans le temps à x fixé
            - 3d plot temps, espace
            - 
        """
        return

class FigureGUI():
    def __init__(self, data, title="", hasToolbar=True, **fig_kw):
        self.data   = data
        self.window = tk.Tk()
        self.title  = None
        if title:
            self.title = tk.Label(self.window, text=title)
            self.title.grid(row=0, column=0, padx=10, pady=10)
        
        fig, ax = plt.subplots(**fig_kw)
        self.figure = fig
        self.ax     = ax
        self.line   = self.ax.plot()
        self.canvas = FigureCanvasTkAgg(self.figure, master=self.window)
        self.canvas.draw()
        self.canvas.get_tk_widget().grid(row=1, column=0, ipadx=40, ipady=20)

        self.toolbar = None
        if hasToolbar:
            # toolbarFrame = tk.Frame(master=self.window)
            # toolbarFrame.grid(row=2, column=0)
            self.toolbar = NavigationToolbar2Tk(self.canvas, self.window, pack_toolbar=False)
            self.toolbar.update()

            # Add Dropdown Menu
    
    def init_plot(self, variable, spaceTime_value, isTime=True):
        self.variable  = variable
        self.isTime    = isTime
        self.spaceTime = spaceTime_value
        self.update_plot()
    
    def set_variable(self, variable):
        self.variable = variable
        self.ax.set_ylabel(variable)
        self.update_plot()
    
    def togglex(self):
        self.isTime = not self.isTime
        if self.isTime:
            self.ax.set_xlabel("Time")
        else:
            self.ax.set_xlabel(self.data.var[0])

        self.update_plot()

    def set_value(self, value):
        self.spaceTime = value
        self.update_plot()

    def update_plot(self):
        """
            variable + time ou space en x et le complémentaire avec une valeur fixée
            Button pour switch time/space
            Slider pour choisir valeur complémentaire

            OutputHandler avec des fonctions juste pour trier et return ce qu'il faut
            Tous les bools et autres dans le Figure GUI
            Penser à éliminer les NaN ou autres s'il y en a
            update_plot called dès qu'on touche à un bouton, slider ou Dropdown menu
            Fonctions callback pour chacun de ces events :
            switch bool pour boutons + change slider
            change value pour slider
            change variable name pour dropdown
            Et dans tous les cas à la fin on call update_plot()
        """
        if self.isTime:
            x = self.data.time
            y = self.data.get(self.variable, space_idx=self.spaceTime)
        else:
            x = self.data.space
            y = self.data.get(self.variable, time_idx=self.spaceTime)
        
        self.line.set_data(x, y)
        self.canvas.draw()
    
    def run(self):
        self.window.mainloop()

# Récupérer le fichier de sortie
data = DataHandler("data_test.out")

# Numpy array :
data.space # array spatial
data.time  # array du temps
data.get("OMEGA")                # array 2D pour la variable Omega
data.get("TEMP", space_idx=0)    # array 1D pour la variable TEMP au point x_{0}
data.get("H", time_idx=-1)       # array 1D pour la variable H au dernier point temporel

