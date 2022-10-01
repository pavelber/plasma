      PROGRAM CHECK
      INTEGER*4 I,J,INDEX(4),COUNT,CBAD
      DOUBLE PRECISION XX(40),COEFF(6), PARAM,X,X0,XM,ALPHA,X1
     &  SASHA,SFD,SFDO,S16
C      CHARACTER
      REAL HARVEST
      COMMON /COEFF/ COEFF
      
      DATA XX /1.00000000001D0, 
     &         1.00001D0, 1.001D0, 1.002D0, 1.004D0, 
     &         1.01, 1.02, 1.05, 1.1, 
     &         1.181, 1.23, 1.3, 1.8, 2.5, 3., 3.572, 4., 4.5,
     &         5., 6., 7., 8., 10., 12., 15.,
     &         20., 25., 30., 40., 50., 70., 100., 200., 300.,
     &         500., 1000., 3119., 10000., 30000., 100000./


      COUNT = 0
      CBAD  = 0

      CALL RANDOM_SEED
      CALL RANDOM_SEED

      OPEN(11,FILE='EXCIT.INP')
      OPEN(12,FILE='check_out')

      READ (11,*)
      READ (11,*)



      DO WHILE(.TRUE.)
         COUNT=COUNT+1
c         print *,COUNT
C--------Read from EXCIT.INP-------------

         READ (11,*, END = 1, ERR = 103) 
     *       (INDEX(I),I=1,4),(COEFF(J),J=1,6)
         
c         PRINT *,' ',INDEX(1),' ',INDEX(2),' ',INDEX(3)
        
        PARAM = 0.D0

	    do kq = 1,80
          if (kq.le.40) then
	 	    x=XX(41-kq)
          else
            CALL RANDOM_NUMBER(HARVEST)
            x=10.D0**(HARVEST*DSQRT(5D0))
          endif
          
 
         SELECT CASE (INDEX(4))

         CASE(0:0)
         CASE(5:5)
             PARAM = COEFF(1)/x+COEFF(2)/x/x+COEFF(3)/x**3+
     &                  COEFF(4)/x**4+COEFF(5)/x*dlog(x)
         CASE(8:8)
             PARAM = (COEFF(5)*X+COEFF(2)+COEFF(3)*X*X)/
     &                  (COEFF(1)+X*X)/X**COEFF(4)
         CASE(11:11)
             PARAM = (COEFF(1)*x*x+COEFF(2)*x+COEFF(3))/
     &                 (x+COEFF(4))**4/x**COEFF(5)
         CASE(16:16)
             XM = COEFF(6)
             ALPHA = 0.7D0 * DSQRT(2.D0*XM/(XM - 1.D0))
             X1 = DSQRT((X-1.D0)/(X+XM))*ALPHA
             S16 = COEFF(1)*DEXP(-X1*X1/COEFF(5)) +
     &        COEFF(2)*DEXP(-(X1-0.333D0)**2/COEFF(5)) +
     &        COEFF(3)*DEXP(-(X1-0.666D0)**2/COEFF(5)) +
     &        COEFF(4)*DEXP(-(X1-1.000D0)**2/COEFF(5))
             PARAM = S16 / X
         END SELECT

c         if (index(2).eq.35.and.index(3).eq.123) print*,x,param

         IF((PARAM.LT.0.)) THEN
c         IF((PARAM.LT.0.).AND.(DABS(PARAM).GT.1.D-30)) THEN
           WRITE (*,'(I5,I8,I8,a,I3,3x,1PE11.3,1PE25.14)') 
     &        INDEX(1),INDEX(2),INDEX(3),
     &       ' METHOD',INDEX(4),PARAM,X
           WRITE (12,'(I5,I8,I8,a,I3,3x,1PE11.3,1PE25.14,2x,6(E26.17))') 
     &        INDEX(1),INDEX(2),INDEX(3),
     &       ' METHOD',INDEX(4),PARAM,X
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
	  stop
      END
      
      DOUBLE PRECISION FUNCTION SASHA(x)
      DOUBLE PRECISION C,x,COEFF
      DIMENSION C(6)
      COMMON /COEFF/ COEFF(6)
      C = COEFF
      SASHA = C(1)/x+C(2)/x/x+C(3)/x**3+C(4)/x**4+C(5)/x*dlog(x)
      RETURN
      END            

      DOUBLE PRECISION FUNCTION SFDO(x)
      DOUBLE PRECISION C,x,COEFF
      DIMENSION C(6)
      COMMON /COEFF/ COEFF(6)
      C = COEFF
      SFDO = (C(1)*x*x+C(2)*x+C(3))/(x+C(4))**4/x**C(5)
      IF (X.EQ.1.D0.AND.SFDO.LT.0.D0)
     &  print *,C(1),C(2),C(3),C(1)+C(2)+C(3)
      RETURN
      END            


      DOUBLE PRECISION FUNCTION SFD(x)
      DOUBLE PRECISION C,x,COEFF
      DIMENSION C(6)
      COMMON /COEFF/ COEFF(6)
      C = COEFF
      SFD = (C(5)*x+C(2)+C(3)*x*X)/(C(1)+X*X)/X**C(4)
      RETURN
      END            

      DOUBLE PRECISION FUNCTION S16(x)
      DIMENSION C(6)
      DOUBLE PRECISION C,X,XM,ALPHA,COEFF
      COMMON /COEFF/ COEFF(6)
      C = COEFF
      XM = C(6)
      ALPHA = 0.7D0 * DSQRT(2.D0*XM/(XM - 1.D0))
      X1 = DSQRT((X-1.D0)/(X+XM))*ALPHA
      S16 = C(1)*DEXP(-X1*X1/C(5)) +
     &        C(2)*DEXP(-(X1-0.333D0)**2/C(5)) +
     &        C(3)*DEXP(-(X1-0.666D0)**2/C(5)) +
     &        C(4)*DEXP(-(X1-1.000D0)**2/C(5))
      S16 = S16 / X
      RETURN
      END            

