import os
import sys
import zipfile
import tempfile
import shutil
import subprocess

output_folder = sys.argv[1]

summary_file_name = "Summary-<station>.ipynb"
data_file = "../data/TripDataByDay.zip"
temp_folder = tempfile.mkdtemp()

fh = open(data_file, 'rb')
z = zipfile.ZipFile(fh)
for name in z.namelist():
    z.extract(name, temp_folder)
fh.close()

data_folder = temp_folder + "/Output"
for f in os.listdir(data_folder):
    if "arrivals" in f or f.startswith("."):
        continue

    station = f.split("_")[0]
    outfile = summary_file_name.replace("<station>", station)
    data_path = os.path.join(data_folder, f)
    output_path = os.path.join(output_folder, outfile)
    with open(summary_file_name, 'rb') as summary_file:
        updated_content = summary_file.read()
        updated_content = updated_content.replace("<arrival_path>", data_path.replace("departures","arrivals"))
        updated_content = updated_content.replace("<departure_path>", data_path)

        with open(output_path, 'wb') as out_file:
            out_file.write(updated_content)

    cmd = "jupyter nbconvert --to html --execute --ExecutePreprocessor.timeout=60000 %s" % output_path
    p = subprocess.Popen(cmd, shell=True)
    p.wait()

shutil.rmtree(temp_folder)

