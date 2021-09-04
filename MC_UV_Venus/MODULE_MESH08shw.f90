! July&August 2007. Principal features:
!1.(August) Varying number of internal grids 
!2.(July) <<< Using the linear interpollation instead of parabolic at the line shape brakes!!!>>>
! Thus a problem for the Doppler shape interpolation has been solved (upper atmosphere).
! ---------------------------------------------------------------------------------------- !
MODULE MESH
INTEGER*4 NT0,NT,NGR,NTR,NTG(20)       
REAL*4 H,H0,DLT4,HG(20) ! Supposed that number of internal grids < 21.
REAL*4, ALLOCATABLE :: RK(:),RKG(:,:),RKGL(:,:),RKGP(:,:) &
! For continuums
,RS296(:),RS260(:),RF296(:),RS(:),RS0(:),RS1(:),RS2(:) &
,AR1(:),AR2(:),AR3(:),AR4(:),RABMA(:,:)
REAL*8 DLT8
! -------------------------------- !
PARAMETER (OBR25=25.0, H_MAX=5.) ! 5. 10.) 
! 25.0-> Line cut off;
! H_MAX -> Step in Far Wings H0 ~ 2-5 cm^-1. 
! -------------------------------- !
END MODULE MESH

SUBROUTINE GRIDS(NTmax,H_MIN,VSTART,V_END) ! <<<Internal grids setting>>>
! *** Attention:                                      ***
! *** V_END may be changed to satisfy resrictions in  ***
! ***  the number of points and the interval width    ***
USE MESH
! USE MOD_IN_GAS_CLOUD
USE INITIAL_ShW_CLOUD 
REAL*8 VSTART,V_END
! Number of internal grids 
      H=H_MIN 
	  HG(1)=H*4.
DO I=2,20
HGG=HG(I-1)+HG(I-1)
IF(HGG>H_MAX)EXIT
HG(I)=HGG
END DO
NGR=I-1 ! Number of internal grids
H0=HG(NGR) ! Real maximal step H0 >= H_MAX

! Calculation interval and the NT0 parameter.
DLT8=V_END-VSTART
IF(DLT8<=0.D0)RETURN
NT0=DLT8/H0 

NP_T=2**(NGR+1)                     
NT00=(NTmax-1)/NP_T
NT0=MIN0(NT0,NT00)
IF(NT0 < 1) NT0=1
DLT8=H0*NT0 ; DLT4=DLT8 ; V_END=VSTART+DLT8
! Dimension of internal grids
NTG(NGR)=NT0
DO I=NGR,2,-1
NTG(I-1)=NTG(I)*2
END DO
NT=NTG(1)*4+1

! WRITE(*,*)VSTART,H0,NT0,DLT4 ;  WRITE(*,*)NGR,HG ;  WRITE(*,*)NT,NTG ; PAUSE

! ****** Allocation ****** !
! Continuum
NTR=NT0*2+1
ALLOCATE (RS296(NTR),RS260(NTR),RF296(NTR),RS(NTR),RS0(NTR) &
                     ,RS1(NTR),RS2(NTR),AR1(NTR),AR2(NTR),AR3(NTR),AR4(NTR),STAT=IERR)
         IF(IERR/=0)THEN
          WRITE(*,*)' Grids-1: Allocaion is wrong !!!'
          STOP
         END IF

! Internal grids
N=NTG(1)
ALLOCATE (RABMA(NT,JMAX), &
RK(NT),RKG(N,NGR),RKGL(N,NGR),RKGP(N,NGR),STAT=IERR)
         IF(IERR/=0)THEN
          WRITE(*,*)'Grids-2: Allocaion is wrong !!!'
          STOP
         END IF
RS296=0. ; RS260=0. ; RF296=0. ; RS=0. ; RS0=0. ; RS1=0. 
RS2=0. ; AR1=0. ; AR2=0. ; AR3=0. ; AR4=0. ; RK=0. ; RKG=0. ; RKGL=0. ; RKGP=0. ;

END  
SUBROUTINE GRIDS_END
USE MESH
! Total Deallocation
DEALLOCATE (RABMA,RS296,RS260,RF296,RS,RS0,RS1,RS2,AR1,AR2,AR3,AR4,RK,RKG,RKGL,RKGP)
END
