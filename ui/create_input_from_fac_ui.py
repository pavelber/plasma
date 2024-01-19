import configparser
import tkinter as tk
from tkinter import filedialog


class UI:
    # Create or load the configuration file

    fac_var: tk.StringVar
    out_var: tk.StringVar
    einstein_var: tk.DoubleVar
    errors_area: tk.Text

    def __init__(self):
        self.config = configparser.ConfigParser()
        self.config_file_path = "form_config.ini"

    def load_config(self):
        try:
            self.config.read(self.config_file_path)
            #    elem_value = config.get("FormValues", "elem_value")
            einstein_value = float(self.config.get("FormValues", "einstein_value"))
            fac_value = self.config.get("FormValues", "fac_value")
            out_value = self.config.get("FormValues", "out_value")
        except:
            einstein_value = ""
            fac_value = ""
            out_value = ""

        return fac_value, out_value, einstein_value

    def save_config(self):
        self.config["FormValues"] = {
            #        "elem_value": elem_var.get(),
            "einstein_value": self.einstein_var.get(),
            "out_value": self.out_var.get(),
            "fac_value": self.fac_var.get()
        }
        with open(self.config_file_path, "w") as configfile:
            self.config.write(configfile)

    def choose_fac_directory(self):
        directory_path = filedialog.askdirectory()
        self.fac_var.set(directory_path)

    def choose_out_directory(self):
        directory_path = filedialog.askdirectory()
        self.out_var.set(directory_path)

    def create_ui(self, run_it):
        # Create the main window
        self.root = tk.Tk()
        self.root.title("Create NOMAD input files from FAC")

        fac_value, out_value, einstein_value = self.load_config()
        self.fac_var = tk.StringVar(value=fac_value)
        self.out_var = tk.StringVar(value=out_value)
        self.einstein_var = tk.DoubleVar(value=einstein_value)

        # Create the text field
        # elem_label = tk.Label(root, text="Element:")
        # elem_label.pack()
        # elem_var = tk.StringVar()
        # elem_entry = tk.Entry(root, textvariable=elem_var)
        # elem_entry.pack()
        # Create the file chooser field
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

    def update(self):
        self.root.update()
    def append_to_errors(self, text):
        self.errors_area.insert(tk.END, text)  #

    def clean_errors(self):
        self.errors_area.delete(1.0, tk.END)

    def get_input_dir(self):
        return self.fac_var.get()

    def get_out_dir(self):
        return self.out_var.get()

    def get_min_eins(self):
        return self.einstein_var.get()
