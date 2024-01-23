import tkinter as tk
from tkinter import filedialog

from ui.generic_ui import GenericUI


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
            spmax_value = int(config.get("FormValues", "spmax_value"))
            spmin_value = int(config.get("FormValues", "spmin_value"))
            nmax_value = int(config.get("FormValues", "nmax_value"))
            osc_value = float(config.get("FormValues", "osc_value"))
            out_value = config.get("FormValues", "out_value")
        except:
            elem_value = "Al"
            spmax_value = 8
            spmin_value = 1
            nmax_value = 6
            osc_value = 1e-8
            out_value = ""
        return elem_value, out_value, nmax_value, osc_value, spmin_value, spmax_value

    def get_config_map(self):
        return {
            "out_value": self.out_var.get(),
            "osc_value": self.osc_var.get(),
            "nmax_value": self.nmax_var.get(),
            "spmin_value": self.spmin_var.get(),
            "spmax_value": self.spmax_var.get(),
            "elem_value": self.elem_var.get()
        }

    def choose_out_directory(self):
        directory_path = filedialog.askdirectory()
        self.out_var.set(directory_path)

    def create_ui(self, run_it):
        super().create_ui(run_it)

        elem_value, out_value, nmax_value, osc_value, spmin_value, spmax_value = self.load_config()
        self.elem_var = tk.StringVar(value=elem_value)
        self.out_var = tk.StringVar(value=out_value)
        self.nmax_var = tk.IntVar(value=nmax_value)
        self.osc_var = tk.DoubleVar(value=osc_value)
        self.spmin_var = tk.IntVar(value=spmin_value)
        self.spmax_var = tk.IntVar(value=spmax_value)

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

        submit_button = tk.Button(self.root, text="Run!", command=run_it)
        submit_button.pack(anchor="w")
        tk.Label(self.root).pack()
        errors_label = tk.Label(self.root, text="Info and errors")
        errors_label.pack(anchor="w")
        self.errors_area = tk.Text(self.root, height=30, width=60)
        self.errors_area.pack(anchor="w")

        self.root.geometry("600x600")
        # Run the application
        self.root.mainloop()

    def get_out_dir(self):
        return self.out_var.get()

    def get_nmax(self):
        return self.nmax_var.get()

    def get_osc(self):
        return self.osc_var.get()

    def get_spmin(self):
        return self.spmin_var.get()

    def get_spmax(self):
        return self.spmax_var.get()

    def get_elem(self):
        return self.elem_var.get()
