      PROGRAM CHECK
      INTEGER*4 I,J,INDEX(4),COUNT,CBAD
      REAL*8 XX(38),COEFF(10), PARAM,X,X0
C      CHARACTER
      REAL HARVEST,HARVEST1
      COMMON /P/ PARAM
      
      DATA XX /1., 1.0001, 1.001, 1.01, 1.03, 1.1, 1.1777, 1.1783, 
     &         1.502, 1.7727, 2., 2.205, 3., 3.572, 4.032, 4.61,
     &         5., 6., 7., 8., 8.852, 10.4, 13.22, 15.,
     &         20., 25., 30., 38., 49., 62.,  90., 129., 
     &         174.69, 455.1, 1100., 2200., 5500., 10000./


      COUNT = 0
      CBAD  = 0

      CALL RANDOM_SEED
      OPEN(11,FILE='RREC.INP')
      OPEN(12,FILE='check_rout')

      READ (11,*)
      READ (11,*)

      CALL RANDOM_SEED

      DO WHILE(.TRUE.)
         COUNT=COUNT+1
c         print *,COUNT
C--------Read from RREC.INP-------------

         READ (11,*, END = 1, ERR = 103) 
     *       (INDEX(I),I=1,4),(COEFF(J),J=1,10)
         
c         PRINT *,' ',INDEX(1),' ',INDEX(2),' ',INDEX(3)
        
        PARAM = 0.D0

	    do kq = 1,500
          if (kq.le.38) then
	 	    x=XX(kq)
          else
            CALL RANDOM_NUMBER(HARVEST)
            x=1000.D0**(HARVEST)
          endif
c          print*,x
 
         SELECT CASE (INDEX(4))
         CASE(4)
             CALL RR(COEFF(1),COEFF(2),COEFF(3),COEFF(4),x)
         END SELECT
         IF((PARAM.LT.0.)) THEN
c         IF((PARAM.LT.0.).AND.(DABS(PARAM).GT.1.D-30)) THEN
           WRITE (*,'(I5,I8,I8,a,I3,3x,2E11.3)') 
     &        INDEX(1),INDEX(2),INDEX(3),
     &       ' METHOD',INDEX(4),PARAM,X
           WRITE (12,'(I5,I8,I8,a,I3,3x,2E11.3)') 
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
