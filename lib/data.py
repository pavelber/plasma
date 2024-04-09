from dataclasses import dataclass


@dataclass
class In1Level:
    level_num: int
    config_1: str
    config_2: str
    energy: float
    e_n0l0: float
    stat_weight: float
    term: str

