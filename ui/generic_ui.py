import configparser
import tkinter as tk
from abc import abstractmethod


class GenericUI:

    @property
    @abstractmethod
    def config_file_path(self):
        pass

    @property
    @abstractmethod
    def title(self):
        pass

    def disable(self):
        self.root.config(cursor="wait")

    def enable(self):
        self.root.config(cursor="")

    def load_config(self):
        config = configparser.ConfigParser()
        config.read(self.config_file_path)
        return self.return_values_from_config(config)

    @abstractmethod
    def return_values_from_config(self, config):
        pass

    def save_config(self):
        config = configparser.ConfigParser()
        config["FormValues"] = self.get_config_map()
        with open(self.config_file_path, "w") as configfile:
            config.write(configfile)

    @abstractmethod
    def get_config_map(self):
        pass

    def create_ui(self, run_it):
        self.root = tk.Tk()
        self.root.title(self.title)

    def update(self):
        self.root.update()

    def append_to_errors(self, text):
        self.errors_area.insert(tk.END, text)  #

    def clean_errors(self):
        self.errors_area.delete(1.0, tk.END)

    def validate_float(self, value):
        try:
            float(value)
            return True
        except ValueError:
            return False

    def on_validate(self, P):
        if self.validate_float(P) or P == "":
            return True
        else:
            self.root.bell()
            return False
