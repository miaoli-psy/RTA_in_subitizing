"""
get base disc profile
"""

import pandas as pd
import ast
import math
from src.common.process_dataframe import insert_new_col_from_n_cols, insert_new_col

def str_to_posi(input_str):
    return ast.literal_eval(input_str)[0]

def cal_ecc(input_posi):
    if isinstance(input_posi,str):
        input_posi = str_to_posi(input_posi)
    return math.sqrt(input_posi[0]**2 + input_posi[1]**2)

def get_far_posi(posi1, posi2):
    if cal_ecc(ast.literal_eval(posi1)[0]) > cal_ecc(ast.literal_eval(posi2)[0]):
        return posi1
    else:
        return posi2

def get_close_posi(posi1, posi2):
    if cal_ecc(ast.literal_eval(posi1)[0]) < cal_ecc(ast.literal_eval(posi2)[0]):
        return posi1
    else:
        return posi2

file = "processed_combined_angle5_with_inputcols.csv"
display = pd.read_csv(file)

insert_new_col_from_n_cols(display, ["center1", "center2"],
                           "far_base", get_far_posi)

insert_new_col_from_n_cols(display, ["center1", "center2"],
                           "close_base", get_close_posi)

insert_new_col(display, "far_base", "far_ecc", cal_ecc)
insert_new_col(display, "close_base", "close_ecc", cal_ecc)


sum_stas_far = display["far_ecc"].describe()
sum_stas_close = display["close_ecc"].describe()
