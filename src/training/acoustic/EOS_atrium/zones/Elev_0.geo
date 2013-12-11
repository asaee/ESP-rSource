*Geometry 1.1,GEN,Elev_0 # tag version, format, zone name
*date Wed Dec 11 07:27:49 2013  # latest file modification 
Elev_0 describes elevator at first floor
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,1.20000,7.20000,0.95000  #   1
*vertex,1.20000,11.30000,0.95000  #   2
*vertex,-0.10000,11.30000,0.95000  #   3
*vertex,-0.10000,7.20000,0.95000  #   4
*vertex,1.20000,7.20000,3.80000  #   5
*vertex,1.20000,11.30000,3.80000  #   6
*vertex,-0.10000,11.30000,3.80000  #   7
*vertex,-0.10000,7.20000,3.80000  #   8
*vertex,1.20000,7.20000,3.55000  #   9
*vertex,1.20000,11.30000,3.55000  #  10
# 
# tag, number of vertices followed by list of associated vert
*edges,6,1,2,10,6,5,9  #  1
*edges,5,2,3,7,6,10  #  2
*edges,4,3,4,8,7  #  3
*edges,5,4,1,9,5,8  #  4
*edges,4,5,6,7,8  #  5
*edges,4,1,4,3,2  #  6
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,Surf-1,VERT,-,-,-,Int_wall,OPAQUE,SIMILAR,00,00  #   1 ||< identical environment
*surf,Surf-2,VERT,-,-,-,Facade_vent,OPAQUE,EXTERIOR,0,0  #   2 ||< external
*surf,Surf-3,VERT,-,-,-,Facade_vent,OPAQUE,EXTERIOR,0,0  #   3 ||< external
*surf,Surf-4,VERT,-,-,-,Facade_vent,OPAQUE,EXTERIOR,0,0  #   4 ||< external
*surf,Surf-5,CEIL,-,-,-,fictious,SC_8985_04nb,ANOTHER,07,06  #   5 ||< Surf-6:Elev_1
*surf,Surf-6,FLOR,-,-,-,concrete,OPAQUE,GROUND,01,00  #   6 ||< ground profile  1
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,6,     5.33 0  # zone base list
