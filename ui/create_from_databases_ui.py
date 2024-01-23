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
            einstein_value = float(config.get("FormValues", "einstein_value"))
            fac_value = config.get("FormValues", "fac_value")
            out_value = config.get("FormValues", "out_value")
        except:
            einstein_value = "1e4"
            fac_value = ""
            out_value = ""
        return fac_value, out_value, einstein_value

    def get_config_map(self):
        return {
            "einstein_value": self.einstein_var.get(),
            "out_value": self.out_var.get(),
            "fac_value": self.fac_var.get()
        }

    # out_dir = os.path.abspath(sys.argv[1])
    #
    # elem = sys.argv[2]
    # nmax = sys.argv[3]
    # osc = sys.argv[4]
    # energy_limits = parse_energy_limits(sys.argv[5])
    # min_sp_num = int(sys.argv[6])
    # max_sp_num = int(sys.argv[7])
    # nucleus_num = int(sys.argv[8])

    def choose_out_directory(self):
        directory_path = filedialog.askdirectory()
        self.out_var.set(directory_path)


    def create_ui(self, run_it):
        super().create_ui(run_it)

        fac_value, out_value, einstein_value = self.load_config()
        self.fac_var = tk.StringVar(value=fac_value)
        self.out_var = tk.StringVar(value=out_value)
        self.einstein_var = tk.DoubleVar(value=einstein_value)

        fac_label = tk.Label(self.root, text="Fac files directory:")
        fac_label.pack(anchor="w")
        fac_entry = tk.Entry(self.root, textvariable=self.fac_var, width=60)
        fac_entry.pack(anchor="w")
        fac_button = tk.Button(self.root, text="Choose Directory", command=self.choose_fac_directory)
        fac_button.pack(anchor="w")
        tk.Label(self.root).pack()
        out_label = tk.Label(self.root, text="Output files directory:")
        out_label.pack(anchor="w")
        out_entry = tk.Entry(self.root, textvariable=self.out_var, width=60)
        out_entry.pack(anchor="w")
        out_button = tk.Button(self.root, text="Choose Directory", command=self.choose_out_directory)
        out_button.pack(anchor="w")
        tk.Label(self.root).pack()
        # Create the float field
        einstein_label = tk.Label(self.root, text="Minimal Einstein Coefficient:")
        einstein_label.pack(anchor="w")
        einstein_entry = tk.Entry(self.root, textvariable=self.einstein_var)
        einstein_entry.pack(anchor="w")
        tk.Label(self.root).pack()
        # Create the button
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

    def get_input_dir(self):
        return self.fac_var.get()

    def get_out_dir(self):
        return self.out_var.get()

    def get_min_eins(self):
        return self.einstein_var.get()
