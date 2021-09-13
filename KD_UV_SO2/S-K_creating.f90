character aaa*6
DIMENSION SK(333),CRS(333)

aaa='.00deg'
open(10,file='K_FDO_'//aaa)
open(11,file='Cr-Sect'//aaa) 

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
write(*,*)'  ***  0-degrees DATA IS ready *** ',N
CLOSE(11)

! ---------------------------------------------------- !
aaa='.75deg'
open(10,file='K_FDO_'//aaa)
open(11,file='Cr-Sect'//aaa) 

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

