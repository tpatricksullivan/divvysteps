import os
import csv
import pandas as pd

from collections import defaultdict
from datetime import datetime


def main(input_folder, output_path):
    d_stations = set()
    a_stations = set()
    d_data = []
    a_data = []
    for f in filter(lambda x: not x.startswith("."), os.listdir(input_folder)):
        print "Working on %s" % f
        file_path = os.path.join(input_folder, f)
        with open(file_path) as input_file:
            reader = csv.reader(input_file)
            departures_by_station, arrivals_by_station = parse_data(reader)
            d_data.append(departures_by_station)
            a_data.append(arrivals_by_station)
            d_stations = d_stations.union(departures_by_station.keys())
            a_stations = a_stations.union(arrivals_by_station.keys())

    arrivals = combine_data(a_stations, a_data)
    departures = combine_data(d_stations, d_data)

    write_data(arrivals, output_path, "arrivals")
    write_data(departures, output_path, "departures")


def parse_data(reader):
    arrivals_by_station = defaultdict(lambda: defaultdict(lambda: 0))
    departures_by_station = defaultdict(lambda: defaultdict(lambda: 0))
    next(reader)
    for line in reader:
        departure_station = line[5]
        arrival_station = line[7]
        try:
            departure_time = datetime.strptime(line[1], "%Y-%m-%d %H:%M")
            arrival_time = datetime.strptime(line[2], "%Y-%m-%d %H:%M")
        except:
            departure_time = datetime.strptime(line[1], "%m/%d/%Y %H:%M")
            arrival_time = datetime.strptime(line[2], "%m/%d/%Y %H:%M")

        departures_by_station[departure_station][
            (datetime(departure_time.year, departure_time.month, departure_time.day))] += 1
        arrivals_by_station[arrival_station][
            (datetime(arrival_time.year, arrival_time.month, arrival_time.day))] += 1

    return departures_by_station, arrivals_by_station


def combine_data(stations, tables):
    filled_series = {}
    for station in stations:
        station_data = {}
        for table in tables:
            try:
                station_data = merge_two_dicts(station_data, table[station])
            except:
                continue
        sorted_station_data = sorted(station_data)
        idx = pd.date_range(sorted_station_data[0], sorted_station_data[-1], freq="D")
        s = pd.Series(station_data)
        s = s.reindex(idx, fill_value=0)
        filled_series[station] = s

    return filled_series


def merge_two_dicts(x, y):
    z = x.copy()
    z.update(y)
    return z


def write_data(stations, output_path, data_type):
    for station in stations:
        stations[station].to_csv(os.path.join(output_path, "%s_%s.csv" % (station, data_type)))


if __name__ == "__main__":
    main("/Users/computer/School/UChicago/Time_Series/Project/CSVs/",
         "/Users/computer/School/UChicago/Time_Series/Project/Output/")