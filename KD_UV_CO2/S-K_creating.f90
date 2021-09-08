!character aaa*6
DIMENSION SK(333),CRS(333)

!aaa='.00deg'
open(10,file='MOLS_SIGMA_FDO_1ANGLE.dat')
open(11,file='Cr-Sect_1ANGLE.dat')

! ***  the first filter: removing lines that correspond to equal and small (more likely zero) downward fluxes !

read(10,*)Z,SK(1),CRS(1),FKO

do i=1,9999
    read(10,*)Z,SK(I),CRS(I),FKN
    if (FKN/=FKO) exit
end do

IMIN=I
FKO=FKN

! ***  the second filter ***  !
IMP=IMIN+1

do I=IMP,9999
    read(10,*)Z,SK(I),CRS(I),FKN
    if (FKN==FKO) exit
    FKO=FKN
end do

IMAX=I

close(10)

N=IMAX-IMIN+1
write(11,*)'  ',N

do I=IMIN,IMAX
    write(11,*)SK(I),CRS(I)
end do

write(*,*)'  ***  0-degrees DATA IS ready *** ',N
close(11)

! ---------------------------------------------------- !
!aaa='.75deg'
open(10,file='MOLS_SIGMA_FDO_1ANGLE.dat')
open(11,file='Cr-Sect_2ANGLE.dat')

! ***  the first filter ***  !
read(10,*)Z,SK(1),CRS(1),FKO
do i=1,9999
read(10,*)Z,SK(I),CRS(I),FKN
IF(FKN/=FKO)EXIT
end do

IMIN=I
FKO=FKN
! ***  the SECOND filter ***  !
IMP=IMIN+1

do I=IMP,9999
read(10,*)Z,SK(I),CRS(I),FKN
IF(FKN==FKO)EXIT
FKO=FKN
end do

IMAX=I
CLOSE(10)
N=IMAX-IMIN+1
write(11,*)'  ',N
DO I=IMIN,IMAX
WRITE(11,*)SK(I),CRS(I)
END DO
write(*,*)'  ***  75-degrees DATA IS ready *** ',N
CLOSE(11)

end

