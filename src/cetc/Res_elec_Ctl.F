C-----------------------------------------------------------------------------------------
C This is a customized controller for purposes of modelling electrical nets featuring
C a battery and (typically) PV in models also using explicit plants for HVAC.
C Unlike conventional ESP-r controllers this controller can sense the state of
C numerous plant and electrical domain components and can actuate numerous plant and
C electrical domain components.
C
C The structure of this controller is customized to quite specific system configurations.
C
C This controller is invoked from ENETSOLV at the beginning of the execution of the
C electric domain.  Consequently, it senses the state of electric domain components
C from the previous solution of the electric domain.  If bps is iterating within the
C time-step between the plant and electrical domains to converge a solution, then the
C controller is sensing the state of the electric domain components at the current
C time-step.  Otherwise, if this is the first pass through the electric domain for the
C current time-step then the controller is sensing the state of electric domain
C components at the previous time-step.
C
C Since the plant domain is solved prior to the invocation of the electric domain,
C the controller is sensing the state of plant components at the current time-step.
C
C The controller actuates a number of plant domain components.  If one of these control
C actions results in a different operational mode for a plant component, a flag is set
C that forces another iteration through the plant and electric domains at the current
C time-step.
C
C This controller is adapted from src/cetc/RE-H2-ctl.F.
C
C For more information on the control strategies see
C   1.
C   X. Neil Saldanha, Carleton U's master thesis <<Li-Ion Battery model, mainly>>
C
C-----------------------------------------------------------------------------------------
C Subroutines
C
C  RES_elec_storage_ctl:   The main control routine. This routine is called in
C                          esrupfs/enetsolv.F, subroutine ENETSOLV and manages the control
C                          schemes available.
C
C  simple_ctl              Basic (simple) control (currently a constant load on battery)
C
C  low_cost                Control based on algorithm to minimise cost. Reads time step
C                          data on electricity costs from an external file ...
C
C  thermal_follow          Control based on (currentl nothing implemented!)
C
C  RES_elec_ctl_read       Reads information required by RES_elec_storage_ctl
C
C  RES_elec_ctl_PostProcess() Writes output to h3k (currently not activated)
C
C-----------------------------------------------------------------------------------------

      SUBROUTINE RES_elec_storage_ctl
      IMPLICIT NONE

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode
      integer   RES_elec_ctl_mode ! control mode 1, 2 or 3, currently only 2 is coded

C << lion battery behaviour very questionable for short time steps -
C    must a time step integration/lag occur for timesteps below a certain
C    threshold value ?? >>


      if (RES_elec_ctl_mode .eq. 1) then
        call simple_ctl          ! call up constant power draw
      elseif (RES_elec_ctl_mode .eq. 2) then
        call low_cost            ! call up low cost
      elseif (RES_elec_ctl_mode .eq. 3) then
        call thermal_follow      ! call up thermal following control
      else                       
        ! <<shouldn't be possible>> do nothing and exit this subroutine
      endif

      END ! End main subroutine


C===================================================================================
C=============================== simple control mode ===============================
C This routine simply applies a constant power draw on each battery found.
C
C <<< common "battery_state" allows for only one battery in a given system!! >>>
C
C It is meant mostly for debugging.
C===================================================================================
      SUBROUTINE simple_ctl
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C Common block definitions
      common/outin/iuout,iuin
      integer   iuout           !- write unit number
      integer   iuin            !- read unit number

C Local variables
      integer IPWC

      character outstr*124

C This control mode has very limited functionality. Warn user.
      write(outstr,'(2A)') 'Renewable energy system (elec)',
     &     ' control mode 1 was entered -- constant battery load!'
      call edisp(iuout,outstr)

      do 10 IPWC=1,NPOWCOM

C     Switch which type of battery is installed.
C     Initiate variables accordingly
        IF( POWCOMID(IPWC) .EQ. pba) THEN      ! Lead-acid battery
C         Demand in Watts imposed on the battery
C         +ve indicates discharging the battery, -ve charge
          batDemandP = 150

        ELSEIF(POWCOMID(IPWC) .EQ. vrb) THEN   ! VRB
C         Demand in Watts imposed on the battery
C         +ve indicates discharging the battery, -ve charge
          batDemandP = 200

        ELSEIF(POWCOMID(IPWC) .EQ. lion) THEN !Lithium-ion battery
C         Demand in Watts imposed on the battery
C         -ve indicates discharging the battery, +ve charge <<???>>
          batDemandP = -300

        ENDIF

   10 continue

      END ! End simple control algorithm


C===================================================================================
C============================ low cost control mode ================================
C
C This routine applies a low cost strategy on the system
C
C Author: A. Geissler
C===================================================================================
      SUBROUTINE low_cost
      IMPLICIT NONE

#include "BATTERY.h"
#include "CETC_definitions.h"

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode
      integer   RES_elec_ctl_mode ! control mode 1, 2 or 3, currently only 2 is coded

C Various RES (elec) controller inputs
      common/reselec_setpts/SOC_PV_ON,SOC_PV_OFF,
     &   SOC_grid_ON,SOC_grid_OFF,fPV_deadband
      real      SOC_PV_ON       !-battery SOC at which PV is used
      real      SOC_PV_OFF      !-battery SOC at which PV use is ended
      real      SOC_grid_ON     !-battery SOC at which grid is used
      real      SOC_grid_OFF    !-battery SOC at which grid use is ended
      real      fPV_deadband    !-value is applied so that PV is used for battery charge
                                !-only when the excess PV power is greater than power to meet
                                !-the loads + power allowance for the heat pump,

C Controller flag information
      common/RESElec_ctl_BatStat/bPV_active,bBattery_charge,
     &                    bBattery_discharge
      logical bPV_active,bBattery_charge,bBattery_discharge

C House non hvac load determined in subroutine POWOC_nonHVACld
      common/Res_elec_ctl/house_load
      real     house_load ! The house load in (W)

C ESP-r COMMONs
c       COMMON/SIMTIM/IHRP,IHRF,IDYP,IDYF,IDWP,IDWF,NSINC,ITS
c       INTEGER IHRP            !-hour of present time-step
c       INTEGER IHRF            !-hour of future time-step
c       INTEGER IDYP            !-year day number of present day
c       INTEGER IDYF            !-year day number of future day
c       INTEGER IDWP            !-day of the week of present day
c       INTEGER IDWF            !-day of the week of future day
c       INTEGER NSINC           !-number of building-side time increments
                               !-since start of simulation
c       INTEGER ITS             !-current building time-step within
                               !-current hour

c       COMMON/PCTIME/TIMSEC
c       REAL    TIMSEC          !-length of time increment (second)

C---- local variables ---------------------------------------------------
      real hvacl, occupantl, generation

C---- function definitions ----------------------------------------------

      real elec_net_load_calc   ! miscellaneous function that retuns data
                                ! from electrical network depending on the
                                ! integer parameter passed to it.



C     Demand in Watts imposed on the battery
C     +ve indicates discharging the battery, -ve charge
C     <<for a pba, for lion, it seems batDemand must change sign (?)>>

      hvacl = elec_net_load_calc(hvac_load)
      occupantl = elec_net_load_calc(occupant_load)

      generation= ABS(elec_net_load_calc(total_gen))

cx      batDemandP = -825.-house_load
      if (generation .gt. (hvacl+occupantl)) then
        batDemandP = 0.
      else
        batDemandP = -1*(hvacl+occupantl-generation)
      endif



      END ! low cost control mode


C===================================================================================
C========================== thermal follow control mode ============================
C fill me in
C===================================================================================
      SUBROUTINE thermal_follow
      IMPLICIT NONE

c #include "building.h"
c #include "plant.h"
c #include "power.h"
c #include "BATTERY.h"

C Common declarations
      common/outin/iuout,iuin
      integer   iuout           !- write unit number
      integer   iuin            !- read unit number

C Local variables
      character outstr*124            !- string containing error message

C This control mode has no functionality. Warn user.
        write(outstr,'(2A)') 'Renewable energy system (elec)',
     &        ' control mode 3 was entered -- *no* functionality!'
        call edisp(iuout,outstr)

      END ! thermal follow


C===================================================================================
C============================ RES_elec_ctl_read ====================================
C
C This subroutine reads the information required by the renewable energy
C systems (elec) controller (subroutine ??? above) and saves the information in
C the appropriate COMMON blocks.
C
C Based on RESH2_Ctl_Read
C===================================================================================
      SUBROUTINE RES_elec_ctl_read
      IMPLICIT NONE
#include "building.h"
#include "plant.h"
#include "power.h"

C-----------------------------------------------------------------------------------------
C COMMON declarations

      common/outin/iuout,iuin
      integer   iuout           !- write unit number
      integer   iuin            !- read unit number

      common/filep/ifil
      integer   ifil            !- basic file unit number

C Input file information
      common/reselecctl/ireselecctl,reselecflnam
      integer ireselecctl       !- flag indicating that file required by renewable energy
                                !- system controller is specified
      character*72 reselecflnam !- name of file containing renewable energy system
                                !- controller inputs

C Basic RES (elec) controller inputs
      common/reselec/RES_elec_ctl_mode
      integer   RES_elec_ctl_mode ! control mode 1, 2 or 3, currently only 2 is coded

C Power-only component information for RES (elec) control
      common/reselec_powoc/IPWC_PVPCU,IPWC_gridconn,IPWC_ACDC,
     &                   IPWC_battery
      integer   IPWC_PVPCU      !-index number of PCU that is connected between the PV and the DC bus
      integer   IPWC_gridconn   !-index number of the grid-connection power-only component
      integer   IPWC_ACDC       !-index number of the DC-AC converter (a PCU power-only component)
      integer   IPWC_battery    !-index number of the battery power-only component

C Plant component information for RES (elec) control
      common/reselec_plt/IPCOMP_heat_pump
      integer   IPCOMP_heat_pump  !-index number of the heat pump component

C Electric node information for RES (elec) control
      common/reselec_enode/IENOD_ACbus,IENOD_DCbus
      INTEGER IENOD_ACbus    ! Elec domain node number of AC bus.
      INTEGER IENOD_DCbus    ! Elec domain Node number of DC bus

C Various RES (elec) controller inputs
      common/reselec_setpts/SOC_PV_ON,SOC_PV_OFF,
     &                 SOC_grid_ON,SOC_grid_OFF,
     &   fPV_deadband
      real      SOC_PV_ON       !-battery SOC at which PV is used
      real      SOC_PV_OFF      !-battery SOC at which PV use is ended
      real      SOC_grid_ON     !-battery SOC at which grid is used
      real      SOC_grid_OFF    !-battery SOC at which grid use is ended
      real      fPV_deadband    !-value is applied so that PV is used for battery charge
                                !-only when the excess PV power is greater than power to meet
                                !-the loads + power allowance for the heat pump,


C-----------------------------------------------------------------------------------------
C Type declarations for local variables.
C-----------------------------------------------------------------------------------------

      character outstr*124            !- string containing error message
      character cErrLoc*32            !- string for error location information

      integer   ireselecfl_ascii      !- input file unit number
      integer   istat, ier            !- error flags
      integer   nd, k

      logical   bFileError            !- boolean signalling file error
      logical   xst                   !- boolean to signal that input file exists

      real      SOC_MAX               !- maximum state of charge of battery
      real      SOC_MIN               !- minimum state of charge of battery

      parameter( SOC_MAX = 100.,
     &           SOC_MIN = 10. )

      integer   lnblnk                ! function returning non-blank string length

C-----------------------------------------------------------------------------------------
C Ensure that renewable energy system (elec) controller file exists.
C-----------------------------------------------------------------------------------------

      bFileError = .false.

      call FINDFIL(reselecflnam,xst)
      if ( .not. xst ) then
C filename is blank or unknown
        write(outstr,'(3A)') 'Renewable energy sys ctl file ',
     &                            reselecflnam(1:lnblnk(reselecflnam)),
     &                            ' was referenced but not found.'
        call edisp(iuout,outstr)
        STOP 'Need renewable energy sys ctl file: unresolvable error.'
      endif


C-----------------------------------------------------------------------------------------
C Read input file of format (example file):
C
C # Comments are denoted by the number sign and are ignored.
C # This file contains inputs required by the renewable energy systems
C # (elec) controller (subroutine RES_elec_ctl).
C # Inputs required by controller in the following order:
C 2            # (0)  control mode (1, 2 or 3) << only '2' usable >>
C 3            # (1)  node number of the AC bus
C 3            # (2)  node number of the DC bus
C 3            # (3)  index number of PCU that is connected between the PV and the DC bus
C 5            # (4)  index number of the grid-connection power-only component
C 6            # (5)  index number of the DC-AC converter (a PCU power-only component)
C 2            # (6)  index number of the battery power-only component
C 1            # (7)  index number of the electrolyzer plant component
C 125          # (8)  battery SOC at which PV is used
C 100          # (9)  battery SOC at which PV use is ended
C -20          # (10) battery SOC at which grid is used
C 20           # (11) battery SOC at which grid use is ended
C 400.         # (12) controller deadband for PV battery charge (W)
C-----------------------------------------------------------------------------------------

C Notify user that renewable energy system (elec) control file is being processed

      call usrmsg( 'Reading RES elec control file ... ',
     &             ' ','P')

C Open the input file
      ireselecfl_ascii = ifil + 41       !- file unit number, should be safe to use

      call erpfree(ireselecfl_ascii, istat)             !- closes file

      call efopseq(ireselecfl_ascii,reselecflnam,1,ier) !- opens a sequential ASCII file

      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error opening renewabel energy sys ctl file. '
         bFileError = .true.
      endif

C     (0)
C     Read the control mode.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'Control mode',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading control mode.'
         bFileError = .true.
         write(cErrLoc,'(A)')'file control mode'
      endif
      k = 0
      call egetwi(outstr,k,RES_elec_ctl_mode,1,3,'W','Control mode',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading control mode.'
         bFileError = .true.
         write(cErrLoc,'(A)')'control mode'
      endif

C     Control mode dependant inputs ...
      if (RES_elec_ctl_mode .eq. 1) then
C     << to be defined >>

      elseif (RES_elec_ctl_mode .eq. 2) then

C     (1)
C     Read the node number of the AC bus.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'AC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading AC bus node number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IENOD_ACbus,1,MENOD,'W','AC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading AC bus node number.'
         bFileError = .true.
      endif

C     (2)
C     Read the node number of the DC bus.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'DC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading DC bus node number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IENOD_DCbus,1,MENOD,'W','DC bus node',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading DC bus node number.'
         bFileError = .true.
      endif

C     (3)
C     Read the index number of the PCU that is connected between the PV and the DC bus.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'pvpcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading PV PCU index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_PVPCU,1,MPOWCOM,'W','pvpcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading PV PCU index number. '
         bFileError = .true.
      endif

C     (4)
C     Read the index number of the grid-connection power-only component.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'grid conn',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*)'File read error reading grid conn index number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_gridconn,1,MPOWCOM,'W','grid conn',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading grid conn index number. '
         bFileError = .true.
      endif

C     (5)
C     Read the index number of the DC-AC converter.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'DC-AC pcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading DC-AC converter',
     &       ' index number . '
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_ACDC,1,MPOWCOM,'W','DC-AC pcu',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading DC-AC converter index number. '
         bFileError = .true.
      endif

C     (6)
C     Read the index number of the battery power-only component.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'battery',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading battery index number.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPWC_battery,1,MPOWCOM,'W','battery',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading battery index number. '
         bFileError = .true.
      endif

C     (7)
C     Read the index number of the heat pump plant component.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                          'file heat pump ipcomp',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading heat pump plant',
     &        ' comp index.'
         bFileError = .true.
      endif
      k = 0
      call egetwi(outstr,k,IPCOMP_heat_pump,1,MPCOM,
     &            'W','heat pump ipcomp',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading heat pump plant comp index.'
         bFileError = .true.
      endif

C     (8)
C     Read the battery SOC at which the PV is used.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'SOC PV on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for PV on.'
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_PV_ON,SOC_MIN,SOC_MAX,
     &            'W','soc elec on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PV on. '
         bFileError = .true.
      endif

C     (9)
C     Read the battery SOC at which PV use is ended.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'SOC PV off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for PV off. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_PV_OFF,SOC_MIN,SOC_MAX,
     &            'W','soc elec off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for PV off. '
         bFileError = .true.
      endif

C     (10)
C     Read the battery SOC at which the grid is used.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,'File SOC grid on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for grid on. '
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_grid_ON,SOC_MIN,SOC_MAX,
     &            'W','soc pem on',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for grid on. '
         bFileError = .true.
      endif

C     (11)
C     Read the battery SOC at which the grid use ends.
      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                                         'File SOC grid off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'File read error reading SOC for grid off.'
         bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,SOC_grid_OFF,SOC_MIN,SOC_MAX,
     &            'W','SOC grid off',ier)
      if ( ier .ne. 0 ) then
         write(iuout,*) 'Error reading SOC for grid off. '
         bFileError = .true.
         write(cErrLoc,'(A)')'SOC grid off'
      endif

C     (12)
C     Read the controller deadband for PV battery loading
      call stripc(ireselecfl_ascii,outstr,0,nd,1,
     &                                  'file power deadband',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'File read error reading deadband for PV bat ld.'
        bFileError = .true.
      endif
      k = 0
      call egetwr(outstr,k,fPV_deadband,0.,1000.,
     &            'W','power deadband',ier)
      if ( ier .ne. 0 ) then
        write(iuout,*) 'Error reading deadband for PV bat ld.'
        bFileError = .true.
        write(cErrLoc,'(A)')'power deadband'
      endif

      elseif (RES_elec_ctl_mode .eq.3) then
C     << to be defined >>

      else
        write(outstr,'(2A)')
     &      ' Undefined mode in renewable energy systems (elec)',
     &      ' control file (first parameter in file).'
        call edisp(iuout,outstr)
        STOP ' Renewable energy sys ctl file: unresolvable error.'
      endif ! mode specific input

C Error handling
      if ( bFileError ) then
        write(outstr,'(3A)') ' Error <',cErrLoc(1:lnblnk(cErrLoc)),
     &            '> in renewable energy systems (elec) control file.'
        call edisp(iuout,outstr)
         STOP ' Renewable energy sys ctl file fatal error '
      endif

C Reposition file to first line.
      rewind(ireselecfl_ascii)

C Notify user that residential h2 cogen control file processing is complete.
      call usrmsg( 'Reading RES elec control file ... done.',
     &             ' ', 'P')

      RETURN
      END

C===================================================================================
C============================= RES_elec_ctl_PostProcess ============================
C
C SYNOPSYS: This routine collects some interesting data pertaining
C to the renewable energy systems (elec) controller, and sends it on to
C h3kreports
C
C===================================================================================
      subroutine RES_elec_ctl_PostProcess()
      use h3kmodule
      implicit none
#include "building.h"
#include "plant.h"
#include "power.h"
#include "BATTERY.h"

C Input file information
      common/reselecctl/ireselecctl,reselecflnam
      integer ireselecctl       !- flag indicating that file required by renewable energy
                                !- system controller is specified
      character*72 reselecflnam !- name of file containing renewable energy system
                                !- controller inputs

C Electric node information
      common/reselec_enode/IENOD_ACbus,IENOD_DCbus
      INTEGER IENOD_ACbus    ! Elec domain node number of AC bus.
      INTEGER IENOD_DCbus    ! Elec domain Node number of DC bus

C Flags indicating status of controller
      common/RESElec_ctl_BatStat/bPV_active,bBattery_charge,
     &                    bBattery_discharge

      logical bPV_active, bBattery_charge, bBattery_discharge

C 'Network interaction'
      real fNetworkBalance

C Return if RESElec control not active.
      if ( iRESElecCtl .ne. 1 ) return

C Get power balance on AC bus (balance = generation + load, loads are -ive)
C
      fNetworkBalance = penodg(IENOD_ACbus) + penodl(IENOD_ACbus)

      IF (mandChargeCycle .EQ. 1) THEN
C When the battery performs a mandatory charge mode, the direction of flow of power through the
C DC/AC converter could be reversed. This will not be possible with the electric current network.
C Instead of changing the electric network, the potential load of the DC node (node 1) is added to the
C network balance of the AC node (node 3) using an average efficiency of the then AC/DC convertor of 98%.
        IENOD_DCBUS = 1
        fNetworkBalance = fNetworkBalance +
     &               (penodg(IENOD_DCbus) + penodl(IENOD_DCbus))/0.98
      ENDIF

      write(*,*)'fNetworkBalance=',fNetworkBalance,' [W?]'

C Determine operating mode, and report data to h3kreports

      if (  bBattery_charge ) then
C.......PV is charging battery
cx        Call AddToReport(rvCtrlReH2NPwBattChrg%Identifier,
cx     &         fNetworkBalance)
      else
cx        Call AddToReport(rvCtrlReH2NPwBattChrg%Identifier,0.0)
      endif

      if ( bBattery_discharge ) then
C.......Battery is responding to loads
cx        Call AddToReport(rvCtrlReH2NPwBattDschrg%Identifier,
cx     &         fNetworkBalance)
      else
cx        Call AddToReport(rvCtrlReH2NPwBattDschrg%Identifier,0.0)
      endif

      if ( bPV_active ) then
C.......Battery is responding to loads
cx        Call AddToReport(rvCtrlReH2NPwPEMFCAct%Identifier,
cx     &         fNetworkBalance)
      else
C.......Battery is responding to loads
cx        Call AddToReport(rvCtrlReH2NPwPEMFCAct%Identifier,0.0)
      endif

      return
      end


C===================================================================================
C============================ RESH2_Ctl_Status_Blockdata ===========================
C
C SYNOPSYS: Initialization of value of bRESElec_ctl_unconverged flag
C
C << copy from Resh2xxx, unclear at this point if actually necessary ... >>
C
C===================================================================================
      blockdata reselec_ctl_status_blockdata
      common / reselec_ctl_status / bRESElec_ctl_unconverged
      logical bRESElec_ctl_unconverged         !-logical flag indicating additional iterations req'd
      data bRESElec_ctl_unconverged/.false./   !initial value
      end blockdata reselec_ctl_status_blockdata

