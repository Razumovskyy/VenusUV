FUNCTION UV_ABSORPTION(WN)
    CHARACTER AAA*5
    PARAMETER (NP=5,BD1=280.0,BD2=400.0)
    DIMENSION S(NP),BDA(NP)

    DATA NBEG/0/
    IF(NBEG==0)THEN
        NBEG=1

        OPEN(8452,FILE='./UV_absorption/UUVA_high_alt_Haus2015.dat')
        3 FORMAT(A5)
        READ(8452,3)AAA
        READ(8452,3)AAA

        DO I=1,NP
            READ(8452,*)BDA(I),S(I)
        END DO

    END IF

    UV_ABSORPTION=0.
    BD=1E7/WN

    !IF(BD>BD2)RETURN                    ! *** start of old Fomin constant approximation for CO2 ***
    !UV_ABSORPTION= S(1)
    !IF(BD<=BD1)RETURN

    !DO I=2,NP
    !IF(BDA(I)>=BD)EXIT
    !END DO
    !C_2=(BDA(I)-BD)/(BDA(I)-BDA(I-1))
    !C_1=1.-C_2
    !UV_ABSORPTION=(C_1*S(I)+C_2*S(I-1)) ! *** end of old Fomin constant approximation for CO2 ***

    IF (BD<BD1 .OR. BD>BD2) RETURN

    DO I=2, NP
        IF (BDA(I)>=BD) EXIT
    END DO

    C_2=(BDA(I)-BD)/(BDA(I)-BDA(I-1))
    C_1=1.-C_2
    UV_ABSORPTION=(C_1*S(I)+C_2*S(I-1))

END



