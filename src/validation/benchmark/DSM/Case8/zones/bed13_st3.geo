*Geometry 1.1,GEN,bed13_st3 # tag version, format, zone name
*date Thu Dec 11 14:14:38 2008  # latest file modification 
bed13_st3 is guest room thirteen on the top level
# tag, X co-ord, Y co-ord, Z co-ord
*vertex,17.50000,6.50000,7.00000  #   1
*vertex,19.00000,6.50000,7.00000  #   2
*vertex,19.00000,8.00000,7.00000  #   3
*vertex,21.00000,8.00000,7.00000  #   4
*vertex,21.00000,11.50000,7.00000  #   5
*vertex,17.50000,11.50000,7.00000  #   6
*vertex,17.50000,6.50000,10.50000  #   7
*vertex,19.00000,6.50000,10.50000  #   8
*vertex,19.00000,8.00000,10.50000  #   9
*vertex,21.00000,8.00000,10.50000  #  10
*vertex,21.00000,11.50000,10.50000  #  11
*vertex,17.50000,11.50000,10.50000  #  12
*vertex,20.20851,11.50000,7.79149  #  13
*vertex,18.29149,11.50000,7.79149  #  14
*vertex,18.29149,11.50000,9.70851  #  15
*vertex,20.20851,11.50000,9.70851  #  16
*vertex,18.50000,8.86000,10.50000  #  17
*vertex,20.00000,8.86000,10.50000  #  18
*vertex,20.00000,10.80000,10.50000  #  19
*vertex,18.50000,10.80000,10.50000  #  20
*vertex,20.15932,11.50000,7.84068  #  21
*vertex,18.34068,11.50000,7.84068  #  22
*vertex,18.34068,11.50000,9.65932  #  23
*vertex,20.15932,11.50000,9.65932  #  24
*vertex,18.62251,9.01844,10.50000  #  25
*vertex,19.87749,9.01844,10.50000  #  26
*vertex,19.87749,10.64156,10.50000  #  27
*vertex,18.62251,10.64156,10.50000  #  28
# 
# tag, number of vertices followed by list of associated vert
*edges,4,1,2,8,7  #  1
*edges,4,2,3,9,8  #  2
*edges,4,3,4,10,9  #  3
*edges,4,4,5,11,10  #  4
*edges,10,5,6,12,11,5,13,16,15,14,13  #  5
*edges,4,6,1,7,12  #  6
*edges,12,7,8,9,10,11,12,7,17,20,19,18,17  #  7
*edges,6,1,6,5,4,3,2  #  8
*edges,10,13,14,15,16,13,21,24,23,22,21  #  9
*edges,10,17,18,19,20,17,25,28,27,26,25  # 10
*edges,4,21,22,23,24  # 11
*edges,4,25,26,27,28  # 12
# 
# surf attributes:
#  surf name, surf position VERT/CEIL/FLOR/SLOP/UNKN
#  child of (surface name), useage (pair of tags) 
#  construction name, optical name
#  boundary condition tag followed by two data items
*surf,ptn_a,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,56,35  #   1 ||< ptn_bg:cirArea_st3
*surf,ptn_b,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,39,03  #   2 ||< ptn_c:bath13-14S3
*surf,ptn_c,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,39,06  #   3 ||< ptn_d:bath13-14S3
*surf,ptn_d,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,53,06  #   4 ||< ptn_e:bed14_st3
*surf,north,VERT,-,WALL,-,Wall_EW_noti,OPAQUE,EXTERIOR,0,0  #   5 ||< external
*surf,ptn_e,VERT,-,PARTN,-,party_wall_n,OPAQUE,ANOTHER,51,04  #   6 ||< ptn_d:bed12_st3
*surf,ceiling,CEIL,-,ROOF,FLAT,Fl_roof_noti,OPAQUE,EXTERIOR,0,0  #   7 ||< external
*surf,floor,FLOR,-,PARTN,-,pa_fl_notion,OPAQUE,ANOTHER,20,07  #   8 ||< ceiling:bed13_st2
*surf,win_frame,VERT,north,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #   9 ||< external
*surf,RL_frame,CEIL,ceiling,F-FRAME,CLOSED,frame_notion,OPAQUE,EXTERIOR,0,0  #  10 ||< external
*surf,win_glz,VERT,win_frame,C-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  11 ||< external
*surf,RL_glz,CEIL,RL_frame,S-WINDOW,CLOSED,window_Not_G,Glaz_notiona,EXTERIOR,0,0  #  12 ||< external
# 
*insol,3,0,0,0  # default insolation distribution
# 
# shading directives
*shad_calc,none  # no temporal shading requested
# 
*insol_calc,none  # no insolation requested
# 
*base_list,1,8,    14.50 0  # zone base list
# 
*bridge_start,0.100,2.080,2.080,20.798  # user fraction,losspercent W/K, total heat loss W/K and total UA W/K
*end_bridge
