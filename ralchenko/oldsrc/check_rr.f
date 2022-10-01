      PROGRAM CHECK
      INTEGER*4 I,J,INDEX(4),COUNT,CBAD
      DOUBLE PRECISION XX(38),COEFF(10), PARAM,X,X0
C      CHARACTER
      REAL HARVEST
      COMMON /P/ PARAM
      
      DATA XX /1., 1.0001, 1.001, 1.01, 1.02, 1.05, 1.1, 
     &         1.2, 1.3, 1.5, 2., 2.5, 3., 3.572, 4., 4.5,
     &         5., 6., 7., 8., 10., 12., 15.,
     &         20., 25., 30., 40., 50., 70., 100., 200., 300.,
     &         500., 1000., 3000., 10000., 30000., 100000./


      COUNT = 0
      CBAD  = 0

      CALL RANDOM_SEED

      OPEN(11,FILE='RREC.INP')
      OPEN(12,FILE='check_rout')

      READ (11,*)
      READ (11,*)



      DO WHILE(.TRUE.)
         COUNT=COUNT+1
c         print *,COUNT
C--------Read from EXCIT.INP-------------

         READ (11,*, END = 1, ERR = 103) 
     *       (INDEX(I),I=1,4),(COEFF(J),J=1,10)
         
c         PRINT *,' ',INDEX(1),' ',INDEX(2),' ',INDEX(3)
        
        PARAM = 0.D0

	    do kq = 1,500
          if (kq.le.38) then
	 	    x=XX(kq)
          else
            CALL RANDOM_NUMBER(HARVEST)
            x=10.D0**(HARVEST*1.5D0)
          endif
 
         SELECT CASE (INDEX(4))
         CASE(4)
             CALL RR(COEFF(1),COEFF(2),COEFF(3),COEFF(4),x)
         END SELECT
         IF((PARAM.LT.0.)) THEN
c         IF((PARAM.LT.0.).AND.(DABS(PARAM).GT.1.D-30)) THEN
           WRITE (*,'(I5,I6,I6,a,I3,3x,2E11.3)') 
     &        INDEX(1),INDEX(2),INDEX(3),
     &       ' METHOD',INDEX(4),PARAM,X
           WRITE (12,'(I5,I6,I6,a,I3,3x,2E11.3)') 
     &        INDEX(1),INDEX(2),INDEX(3),
     &       ' METHOD',INDEX(4),PARAM,X
           CBAD = CBAD+1
		   goto 22
         ENDIF
        enddo
 22	   continue
      ENDDO   
 1    CLOSE(10)
C      PRINT *,(INDEX(I),I=1,4),(COEFF(J),J=1,7)
      PRINT *," There are ",COUNT," lines with" ,CBAD," bad lines"
      STOP
103	  print *,'error in line: ',count
	  stop
      END
      
      SUBROUTINE RR(C1,C2,C3,C4,x)
      DOUBLE PRECISION C1,C2,C3,C4,PARAM,x
      COMMON /P/ PARAM
      PARAM = (C1+C2/x+C3/x/x)*x**(-3.5-C4)
      RETURN
      END            
