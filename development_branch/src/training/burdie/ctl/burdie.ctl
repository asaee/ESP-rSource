proj cntrl  # overall descr 
* Mass Flow
no descrip  # flow descr 
   2  # No. of controls
* Control mass
# measures node (  0)             
   -4    0    0    0  # sensor data
# actuates mass flow component:   5 duct
   -4    5    3  # actuator data
    1 # No. day types
    1  365  # valid Sat  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
    1    0   7.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  20.000 1.000
    1    0  19.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
liv           s_ext         win2          liv         
kitch         n_ext         win2          kitch       
bed1          s_ext         win2          bed1        
* Control mass
# measures node (  0)             
   -4    0    0    0  # sensor data
# actuates mass flow component:   7 fan
   -4    7    1  # actuator data
    1 # No. day types
    1  365  # valid Sat  1 Jan - Sun 31 Dec
     3  # No. of periods in day
    1    0   0.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
    1    0   9.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  0.000 1.000
    1    0  17.000  # ctl type (dry bulb > flow), law (on / off), start @
      2.  # No. of data items
  100.000 1.000
coll          hal_up        fan           hal_up      
