bld_simple_ufh.ctl  # overall descr 
* Building
underfloor heating system, free float until 7h, use 10kW to reach 15C at 7h and20C at 9h. No heating on weekend. 
   1  # No. of functions
* Control function
# measures the temperature of the current zone.
    0    0    0    0  # sensor data
# actuates within surface  9 in zone. 1
    1    9    7  # actuator data
    0 # No. day types
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     4  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    0    1   7.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 0.000 0.000 15.000 100.000
    0    1   9.000  # ctl type, law (ideal control), start @
      6.  # No. of data items
  10000.000 0.000 0.000 0.000 20.000 100.000
    0    2  18.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
    1  365  # valid Sun  1 Jan - Sun 31 Dec
     1  # No. of periods in day
    0    2   0.000  # ctl type, law (free floating), start @
      0.  # No. of data items
# Function:Zone links
  1
