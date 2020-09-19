#import fitdecode
#
#with fitdecode.FitReader('A7IB3702.FIT') as fit:
#    for frame in fit:
#        # The yielded frame object is of one of the following types:
#        # * fitdecode.FitHeader
#        # * fitdecode.FitDefinitionMessage
#        # * fitdecode.FitDataMessage
#        # * fitdecode.FitCRC
#
#        if isinstance(frame, fitdecode.FitDataMessage):
#            # Here, frame is a FitDataMessage object.
#            # A FitDataMessage object contains decoded values that
#            # are directly usable in your script logic.
#            print(frame.name)
#            for i in frame:
#                print(i.name, i.value)


import csv
import os
#to install fitparse, run
#sudo pip3 install -e git+https://github.com/dtcooper/python-fitparse#egg=python-fitparse
import fitparse
import pytz

allowed_fields = ['timestamp','position_lat','position_long', 'distance',
        'enhanced_altitude', 'altitude','enhanced_speed',
        'speed', 'heart_rate','cadence','fractional_cadence']
required_fields = ['timestamp', 'position_lat', 'position_long', 'altitude']

##

from activityio import fit
data = fit.read('/run/Parts/Exch/Sync/swap/Garmin/ACTIVITY/A8JG5519.FIT')

#max(data.speed *3600/1000)
data.cad.base_unit

x = fit.gen_records('/run/Parts/Exch/Sync/swap/Garmin/ACTIVITY/A8JG5519.FIT')

data
##
for i in x:
    print(i)
##





#import csv
#import os
##to install fitparse, run
##sudo pip3 install -e git+https://github.com/dtcooper/python-fitparse#egg=python-fitparse
#import fitparse
#import pytz
#
#allowed_fields = ['timestamp','position_lat','position_long', 'distance',
#'enhanced_altitude', 'altitude','enhanced_speed',
#                 'speed', 'heart_rate','cadence','fractional_cadence']
#required_fields = ['timestamp', 'position_lat', 'position_long', 'altitude']
#
#UTC = pytz.UTC
#CST = pytz.timezone('US/Central')
#
#def main():
#    files = os.listdir()
#    fit_files = [file for file in files if file[-4:].lower()=='.fit']
#    for file in fit_files:
#        new_filename = file[:-4] + '.csv'
#        if os.path.exists(new_filename):
#            #print('%s already exists. skipping.' % new_filename)
#            continue
#        fitfile = fitparse.FitFile(file,
#            data_processor=fitparse.StandardUnitsDataProcessor())
#
#        print('converting %s' % file)
#        write_fitfile_to_csv(fitfile, new_filename)
#    print('finished conversions')
#
#


required_fields = ['timestamp', 'start_time', 'total_elapsed_time', 'total_timer_time', 'total_distance', 'total_cycles', 'unknown_110', 'time_standing', 'total_calories', 'enhanced_avg_speed', 'avg_speed', 'enhanced_max_speed', 'max_speed', 'avg_power', 'max_power',  'first_lap_index', 'num_laps', 'intensity_factor', 'left_right_balance', 'avg_stroke_distance', 'pool_length', 'threshold_power', 'num_active_lengths', 'unknown_80',  'avg_stance_time_percent',  'stand_count', 'event', 'event_type', 'sport', 'sub_sport',  'avg_cadence', 'total_training_effect', 'pool_length_unit']


fitfile = fitparse.FitFile('A7IB3702.FIT',data_processor=fitparse.StandardUnitsDataProcessor())
messages = fitfile.messages
data = []
for m in messages:
    if not hasattr(m, 'fields'):
        continue

    mdata = {}
    for field in m.fields:
        if field.name in required_fields:
            mdata[field.name] = field.value

    if len(mdata) > 10:
        data.append(mdata)

for k in data:
    print(k)



#'timestamp', 'start_time', 
#'total_elapsed_time': 229.304, 'total_timer_time': 229.304, 'total_distance': 200.0, 'total_cycles': 92, 'total_calories': 45, 'enhanced_avg_speed': 3.1392, 
#'avg_speed': 3139.2000000000003, 'enhanced_max_speed': 3.9024000000000005, 'max_speed': 3902.4, 'num_lengths': 4, 'first_length_index': 0, 'avg_stroke_distance': 2.17, 
#'num_active_lengths': 4, 'event': 'lap', 'event_type': 'stop', 'lap_trigger': 'manual', 'sport': 'swimming', 'swim_stroke': 'mixed', 'sub_sport': 'lap_swimming'












#def write_fitfile_to_csv(fitfile, output_file='test_output.csv'):
#    messages = fitfile.messages
#    data = []
#    for m in messages:
#        skip=False
#        if not hasattr(m, 'fields'):
#            continue
#        fields = m.fields
#        #check for important data types
#        mdata = {}
#        for field in fields:
#            if field.name in allowed_fields:
#                if field.name=='timestamp':
#                    mdata[field.name] = UTC.localize(field.value).astimezone(CST)
#                else:
#                    mdata[field.name] = field.value
#        for rf in required_fields:
#            if rf not in mdata:
#                skip=True
#        if not skip:
#            data.append(mdata)
#    #write to csv
#    with open(output_file, 'w') as f:
#        writer = csv.writer(f)
#        writer.writerow(allowed_fields)
#        for entry in data:
#            writer.writerow([ str(entry.get(k, '')) for k in allowed_fields])
#    print('wrote %s' % output_file)
#
#if __name__=='__main__':
#    main()
