      PROGRAM CHECK
      INTEGER*4 I,J,INDEX(4),COUNT,CBAD
      DOUBLE PRECISION XX(40),COEFF(4), PARAM,X,X0,XM,ALPHA,X1
     &  SASHA,SFD,SFDO,S16
C      CHARACTER
      REAL HARVEST
      COMMON /COEFF/ COEFF
      
      DATA XX /1.00000000001D0, 
     &         1.00001D0, 1.001D0, 1.002D0, 1.004D0, 
     &         1.01, 1.02, 1.05, 1.1, 
     &         1.2, 1.3, 1.415, 1.7, 2., 2.785, 3., 3.572, 4., 4.5,
     &         5., 6.5, 8., 10., 12., 15.,
     &         20., 25., 30., 40., 50., 64.95, 100., 200., 300.,
     &         500., 1000., 3000., 10000., 30000., 100000./


      COUNT = 0
      CBAD  = 0

      CALL RANDOM_SEED
      CALL RANDOM_SEED

      OPEN(11,FILE='BCFP.INP')
      OPEN(12,FILE='check_bcfp')

      READ (11,*)
      READ (11,*)



      DO WHILE(.TRUE.)
         COUNT=COUNT+1
c         print *,COUNT
C--------Read from BCFP.INP-------------

         READ (11,*, END = 1, ERR = 103) 
     *       (INDEX(I),I=1,4),(COEFF(J),J=1,4)
         
c         PRINT *,' ',INDEX(1),' ',INDEX(2),' ',INDEX(3)
        
        PARAM = 0.D0

        IF (DABS(COEFF(2)).LT.1.D-50.AND.
     *      DABS(COEFF(3)).LT.1.D-50.AND.
     *      DABS(COEFF(4)).LT.1.D-50) CYCLE

	    do kq = 1,100
          if (kq.le.40) then
	 	    x=XX(41-kq)
          else
            CALL RANDOM_NUMBER(HARVEST)
            x=18.D0**(2.D0*HARVEST*harvest)
          endif
          
          IF (COEFF(2).LT.0.D0) THEN
            Y = 1.D0 - 1.D0/X
            PARAM = -COEFF(2)*DLOG(X)+COEFF(3)*Y*Y+
     *               COEFF(4)*Y/X+COEFF(1)*Y/X/X
          ELSE
            PARAM = COEFF(2)/(1.D0 + COEFF(3)/X/X + COEFF(4)/X)
          ENDIF
 
         IF((PARAM.LT.0.)) THEN
c         IF((PARAM.LT.0.).AND.(DABS(PARAM).GT.1.D-30)) THEN
           WRITE (*,'(I5,I8,I8,I6,3x,1PE11.3,1PE25.14)') 
     &        INDEX(1),INDEX(2),INDEX(3),
     &        INDEX(4),PARAM,X
           WRITE (12,'(I5,I8,I8,I6,3x,1PE11.3,1PE25.14,2x,6(E26.17))') 
     &        INDEX(1),INDEX(2),INDEX(3),
     &        INDEX(4),PARAM,X
           CBAD = CBAD+1
		   goto 22
         ENDIF
        enddo

 22	    continue

      ENDDO   

 1    CLOSE(10)
C      PRINT *,(INDEX(I),I=1,4),(COEFF(J),J=1,7)
      PRINT *," There are ",COUNT," lines with" ,CBAD," bad lines"
      STOP
103	  print *,'error in line: ',count

      CLOSE (11)
      CLOSE (12)

	  stop
      END
      
