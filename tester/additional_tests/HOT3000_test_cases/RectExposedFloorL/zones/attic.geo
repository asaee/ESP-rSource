# geometry of attic defined in: user/RectExposedFloorL/zones/attic.geo
GEN  attic  describes the roof or attic zone  # type, name, descr
       6        5 0.000    # vertices, surfaces, rotation angle
#  X co-ord, Y co-ord, Z co-ord
    0.00000  0.00000  2.50000   # vert 1  
    0.00000  10.00000  2.50000   # vert 2  
    10.00000  10.00000  2.50000   # vert 3  
    10.00000  0.00000  2.50000   # vert 4  
    5.00000  2.08500  4.58500   # vert 5  
    5.00000  7.91500  4.58500   # vert 6  
# no of vertices followed by list of associated vert
   4,   1,   2,   3,   4,
   4,   1,   5,   6,   2,
   3,   2,   6,   3,
   4,   3,   6,   5,   4,
   3,   4,   5,   1,
# unused index
 0   0   0   0   0  
# surfaces indentation (m)
0.000 0.000 0.000 0.000 0.000 
    3    0    0    0  # default insolation distribution
# surface attributes follow: 
# id surface       geom  loc/   mlc db      environment
# no name          type  posn   name        other side
  1, to_main       OPAQ  FLOR  ceiling_r    main           
  2, WestSlope     OPAQ  SLOP  asphalt      EXTERIOR       
  3, NorthEnd      OPAQ  VERT  asphalt      EXTERIOR       
  4, EastSlope     OPAQ  SLOP  asphalt      EXTERIOR       
  5, SouthEnd      OPAQ  VERT  asphalt      EXTERIOR       
# base
1   0   0   0   0   0   100.00   
