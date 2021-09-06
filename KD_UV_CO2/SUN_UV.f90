FUNCTION SUN(V)

    CHARACTER UVNAME*70
    PARAMETER (NWL=580,WL1=120.5,WL2=699.5)
    DIMENSION SIR(NWL)
    SAVE SIR

    DATA IBG/-1/
    IF(IBG==-1)THEN
        IBG=1

        OPEN(10457,FILE='UV-name.txt')
        READ(10457,1)UVNAME
        1 FORMAT(A70)
        CLOSE(10457)
        WRITE(*,*)UVNAME !; PAUSE 0

        OPEN(10457,FILE=UVNAME)
        DO I=1,NWL
        READ(10457,*)A,SIR(I)
        END DO
        CLOSE(10457)
    END IF

    BD=1E7/V
    SUN=0.
    IF(BD<WL1.OR.BD>=WL2)RETURN

    I=(BD-WL1)+1                      ! /1.0
    S1=SIR(I)
    S2=SIR(I+1)
    CCC2=BD-(I-1)-WL1 ; CCC1=1.-CCC2
    SUN=S1*CCC1+S2*CCC2

    SUN=SUN/(1e10/BD**2)*1.93

END
