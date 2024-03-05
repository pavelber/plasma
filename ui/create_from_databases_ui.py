import tkinter as tk
from tkinter import filedialog, simpledialog

from ui.generic_ui import GenericUI


class EnergyLimitsDialog(simpledialog.Dialog):

    def __init__(self, minsp, maxsp, current_data, parent):
        self.minsp = minsp
        self.maxsp = maxsp
        self.limits = {k.strip(): v.strip() for k, v in (pair.split(':') for pair in current_data.split(','))}
        super().__init__(parent)
        self.title = "Energy Limits"

    def body(self, master):
        self.entries = []
        self.vars = {}
        for i in range(self.minsp, self.maxsp + 1):
            label = tk.Label(master, text="{} max energy: ".format(i))
            label.grid(row=i, column=0)
            k = str(i)
            if k in self.limits:
                v = float(self.limits[k])
            else:
                v = 1000.0
            energy_var = tk.DoubleVar(master, value=v)
            self.vars[i] = energy_var
            entry = tk.Entry(master, textvariable=energy_var, width=7)
            entry.grid(row=i, column=1)
            self.entries.append(entry)

    def apply(self):
        # self.result = [entry.get() for entry in self.entries]
        self.result = {k: v.get() for k, v in self.vars.items()}


class CreateFromDataBasesUI(GenericUI):

    @property
    def config_file_path(self):
        return "create_from_databases_config.ini"

    @property
    def title(self):
        return "Create NOMAD input files from databases"

    def return_values_from_config(self, config):
        try:
            elem_value = config.get("FormValues", "elem_value")
        except:
            elem_value = "Al"
        try:
            spmax_value = int(config.get("FormValues", "spmax_value"))
        except:
            spmax_value = 8
        try:
            spmin_value = int(config.get("FormValues", "spmin_value"))
        except:
            spmin_value = 1
        try:
            nmax_value = int(config.get("FormValues", "nmax_value"))
        except:
            nmax_value = 6
        try:
            out_value = config.get("FormValues", "out_value")
        except:
            out_value = ""
        try:
            energy_limits_value = config.get("FormValues", "energy_limits_value")
        except:
            energy_limits_value = "1:70.8,2:150,3:250,4:350,5:450,6:550,7:750,8:1000"

        return elem_value, out_value, nmax_value, spmin_value, spmax_value, energy_limits_value

    def get_config_map(self):
        return {
            "out_value": self.out_var.get(),
            "nmax_value": self.nmax_var.get(),
            "spmin_value": self.spmin_var.get(),
            "spmax_value": self.spmax_var.get(),
            "elem_value": self.elem_var.get(),
            "energy_limits_value": self.energy_limits_var.get(),
        }

    def open_dialog(self):
        modal_dialog = EnergyLimitsDialog(self.spmin_var.get(), self.spmax_var.get(), self.energy_limits_var.get(),
                                          self.root)

        result = modal_dialog.result
        self.energy_limits_var.set(', '.join([f"{key}:{value}" for key, value in result.items()]))

    def choose_out_directory(self):
        directory_path = filedialog.askdirectory()
        self.out_var.set(directory_path)

    def create_ui(self, run_it):
        super().create_ui(run_it)

        elem_value, out_value, nmax_value, spmin_value, spmax_value, energy_limits_value = self.load_config()
        self.elem_var = tk.StringVar(value=elem_value)
        self.out_var = tk.StringVar(value=out_value)
        self.nmax_var = tk.IntVar(value=nmax_value)
        self.spmin_var = tk.IntVar(value=spmin_value)
        self.spmax_var = tk.IntVar(value=spmax_value)
        self.energy_limits_var = tk.StringVar(value=energy_limits_value)

        elem_label = tk.Label(self.root, text="Element")
        elem_label.pack(anchor="w")
        elem_entry = tk.Entry(self.root, textvariable=self.elem_var)
        elem_entry.pack(anchor="w")
        tk.Label(self.root).pack()

        out_label = tk.Label(self.root, text="Output files directory:")
        out_label.pack(anchor="w")
        out_entry = tk.Entry(self.root, textvariable=self.out_var, width=60)
        out_entry.pack(anchor="w")
        out_button = tk.Button(self.root, text="Choose Directory", command=self.choose_out_directory)
        out_button.pack(anchor="w")
        tk.Label(self.root).pack()

        nmax_label = tk.Label(self.root, text="N Max")
        nmax_label.pack(anchor="w")
        nmax_entry = tk.Entry(self.root, textvariable=self.nmax_var)
        nmax_entry.pack(anchor="w")
        tk.Label(self.root).pack()

        spmin_label = tk.Label(self.root, text="First spectroscopic number")
        spmin_label.pack(anchor="w")
        spmin_entry = tk.Entry(self.root, textvariable=self.spmin_var)
        spmin_entry.pack(anchor="w")
        tk.Label(self.root).pack()

        spmax_label = tk.Label(self.root, text="Last spectroscopic number")
        spmax_label.pack(anchor="w")
        spmax_entry = tk.Entry(self.root, textvariable=self.spmax_var)
        spmax_entry.pack(anchor="w")
        tk.Label(self.root).pack()

        energy_limits_label = tk.Label(self.root, text="Energy Limits")
        energy_limits_label.pack(anchor="w")
        energy_limits_entry = tk.Entry(self.root, textvariable=self.energy_limits_var, width=70)
        energy_limits_entry.pack(anchor="w")
        energy_limits_button = tk.Button(self.root, text="Change Limits", command=self.open_dialog)
        energy_limits_button.pack(anchor="w")
        tk.Label(self.root).pack()

        submit_button = tk.Button(self.root, text="Run!", command=run_it)
        submit_button.pack(anchor="w")
        tk.Label(self.root).pack()
        errors_label = tk.Label(self.root, text="Info and errors")
        errors_label.pack(anchor="w")
        self.errors_area = tk.Text(self.root, height=40, width=80)
        self.errors_area.pack(anchor="w")

        self.root.geometry("600x800")
        # Run the application
        self.root.mainloop()

    def get_out_dir(self):
        return self.out_var.get()

    def get_nmax(self):
        return self.nmax_var.get()

    def get_spmin(self):
        return self.spmin_var.get()

    def get_spmax(self):
        return self.spmax_var.get()

    def get_elem(self):
        return self.elem_var.get()
