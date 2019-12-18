from mp_dash import insert_sensor_data
from PA import master_sensor
from time import sleep

while True:
    for key in list(master_sensor.keys()):
        insert_sensor_data(key)
    sleep(300)
