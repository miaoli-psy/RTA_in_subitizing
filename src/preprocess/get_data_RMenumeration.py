import os
import pandas as pd
import numpy as np
import ast

from src.common.process_dataframe import insert_new_col_from_n_cols


def get_resp_n(input_string):
    return int(input_string.split('_')[-1])

def get_deviaion_score(reportN, numerosity):
    return reportN - numerosity

def get_avg_ecc(posis_str):
    """ average eccentricity  across all discs."""
    try:
        coords = ast.literal_eval(posis_str)
        eccentricities = [np.sqrt(x**2 + y**2) for x, y in coords]
        return np.mean(eccentricities)
    except:
        return np.nan

if __name__=='__main__':
    to_csv = False
    PATH = "../../data/enumeration/raw_data/"

    files = os.listdir(PATH)

    # read raw data files
    csv_files = [os.path.join(PATH, file) for file in files if file.endswith(".csv")]
    dataframe_list = [pd.read_csv(file_name) for file_name in csv_files]

    # combine
    totalData = pd.concat(dataframe_list, ignore_index=True)

    totalData = totalData.dropna(subset=['rotation_index'])

    # add posis column based on rotation_index
    totalData['posis'] = totalData.apply(
        lambda row: row[f"rotated_posis_{int(row['rotation_index'])}"], axis=1
    )

    # # col names
    # all_column_names = totalData.columns.tolist()

    # keep cols
    columns_to_keep = [
        'protectzonetype',
        'numerosity',
        'type',
        'key_resp.rt',
        'key_resp.keys',
        'blocks_rm.thisN',
        'rotation_index',
        'participant',
        'sex',
        'posis',
        'age']

    totalData = totalData[columns_to_keep]

    # remove rows with NaN
    totalData = totalData.dropna(subset= ['key_resp.keys'])

    # add col deviation score
    insert_new_col_from_n_cols(totalData,
                              ["key_resp.keys"],
                              "reportedN", get_resp_n)

    insert_new_col_from_n_cols(totalData,
                              ["reportedN", "numerosity"],
                              "deviation", get_deviaion_score)

    # add col average eccentricity
    totalData['avg_ecc'] = totalData['posis'].apply(get_avg_ecc)

    # pixels to visual degrees
    totalData['avg_ecc'] = totalData['avg_ecc'] * 0.0273


    if to_csv:
        totalData.to_csv('data_RMenumeration.csv', index = False)
        totalData.sample(30).to_csv('data_RMenumeration_sample30.csv', index=False)

