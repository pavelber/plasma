      PROGRAM FIT
      IMPLICIT REAL*8 (A-H,O-Z)
      DOUBLE PRECISION X(5),FVEC(1000),TOL,WA(1000),EX(1000),
     &                 XNEW(1000),E(1000),CS(1000),P(5),P5(5)
      INTEGER N,M,LWA,INFO,IWA(5),INDEX,COUNT,MODE,POW
      INTEGER NLOW,NUPP,SPCH,ICHECK,NUMB_RUNS
      INTEGER GOOD_FIT(50000,50000)
      EXTERNAL FCN_SASHA,FCN_15,FCN_16
      EXTERNAL LMDIF1
      CHARACTER*40 LINE
      CHARACTER*12 BAD_OUT
      CHARACTER*11 FILENAME
      CHARACTER*11 INF
      CHARACTER*9 OUT_INF
      CHARACTER*10 OUT_DATA
      CHARACTER*1 APAR
      COMMON /BLOCKA/ EX,CS, XNEW
      COMMON /INDTOT/INDTOT

      CALL RANDOM_SEED()

C=======Constant parameters====================================      
      N      = 5
      LWA    = 1000
      MAXCHK = 100
      FACTOR = 8.D0
      FACTOR1 = 3.D0
C==============Default values==================================
      INF="info_ex.dat"
C=============Selecting mode===================================

      PRINT *," Enter the spectroscopic charge:"
      READ (5, FMT=*) SPCH
      
      NUMB_RUNS = 10
      TOL = 1d-05
c ============ variance ===============
      vari = 0.1D0

      
          
      POW = 0.28D0
          
      N_TOT = 0
      N_GOOD = 0

c ==========  INPUT ============
      OPEN (10,FILE='excit')
      COUNT=0

      OPEN (12,FILE='outinf.dat')
          
      OPEN (13,FILE='outpp.dat')
      WRITE (13,*)
      WRITE (13,*)

      OPEN (15,FILE='outpp_da.dat')
      WRITE (15,*)
      WRITE (15,*)

      OPEN (14,FILE='badout.dat')

 11   FORMAT("badout.dat")
C================Global Loop===================================
      PRINT *,'starting loop...'

c  X below is for a tab
6634  FORMAT(I6,4X,I6,4X,I1,4X,1PE10.3)
6635  FORMAT(1PE10.3,2X,1PE10.3,2X,1PE10.3)

      DO NRUN = 1, NUMB_RUNS

          DO
C-----------------Read from info.dat---------------------------
             READ (10,*,END=56,ERR=111) NLOW,NUPP,INDEX,OSC
c       print*,NLOW,NUPP
             IF (NRUN.EQ.1) N_TOT = N_TOT + 1
             IF (GOOD_FIT(NLOW,NUPP) .EQ. 1) CYCLE

             COUNT=COUNT+1
c             print*,"---- ",NLOW,NUPP,INDEX,OSC

c                pause
C-----------------Read from data file--------------------------
             INDTOT=1

             OLDCS = 1.D100
             OLDSLP = 0.D0
             
             XNEW = 0.D0
             EX = 0.D0
             CS = 0.D0
             E  = 0.D0
             CS0 = 0.D0

             
             DO 
               READ(10,'(A40)',ERR=21,END=56) LINE
             
               IF (LINE(1:2).EQ.'--') EXIT
             
c             READ(10,6635,ERR=21,END=56) EX(INDTOT),CS(INDTOT),E(INDTOT)

               READ(LINE,6635) EX(INDTOT),CS(INDTOT),E(INDTOT)
c               print*,EX(INDTOT),CS(INDTOT),E(INDTOT)
               
c               IF (CS(INDTOT).GT.OLDCS.AND.INDTOT.GT.5) CYCLE
               
               
               IF (CS(INDTOT).GT.CS0) CS0=CS(INDTOT)
               
               SLOPE = 0.D0
               IF (INDTOT.GT.2) SLOPE = DLOG(CS(INDTOT)/CS(INDTOT-2))/
     &                     DLOG(EX(INDTOT-2)/EX(INDTOT))
               
               IF ((INDTOT.GT.8.AND.OLDCS.LT.CS(INDTOT)) .OR.
     &             (INDTOT.GT.7.AND.DABS(SLOPE-OLDSLP).GT.2.D0)) THEN
                 DO
                   READ(10,'(A40)',ERR=21,END=56) LINE
                   IF (LINE(1:2).EQ.'--') GOTO 21 
                 ENDDO
               ENDIF
               OLDCS = CS(INDTOT)
               OLDX = EX(INDTOT)
               OLDSLP = SLOPE
               INDTOT=INDTOT+1
               IF (INDTOT.GT.1000) THEN 
                 PRINT *,'INDTOT = ',INDTOT
   		         STOP 'ARRAY SIZE EXCEEDED...'
               ENDIF
             ENDDO
c               print*,'slope:',slope

 21          CONTINUE

             BACKSPACE 10

             M=INDTOT-1

c check last two points
             DO KW = 1,2
               IF (CS(M).GT.CS(M-1)) M = M - 1
             ENDDO
             
c ------------------------------------------
c ----- skip if too few points -------------
             IF (M.LE.5) CYCLE

c -------- rescale cross sections ----------


c -- first, do the coefficient

             ALCOEF = 0.7D0 * DSQRT(2.D0*EX(M)/(EX(M)-1.D0))

             DO 30 J=1,M
               CS(J)=CS(J)/CS0*EX(J)
               XNEW(J) = DSQRT((EX(J)-1.D0)/(EX(J)+EX(M)))*ALCOEF
               
c               print*,XNEW(J),CS(J)*CS0
 30          CONTINUE


            


C--------------------------------------------------------------
C------------ dipole-allowed ----------------------------------
            IF (INDEX.EQ.0) THEN

C--------------------Initial guess---------------------------
               X(1)= 1.*10.**POW
               X(2)= 1.*10.**POW
               X(3)=-1.*10.**POW
               X(4)= 1.*10.**POW
               X(5)= 0.1*10.**POW

               IME = 5

               CALL LMDIF1(FCN_SASHA,M,N,X,FVEC,TOL,INFO,IWA,WA,LWA)

               P5(1)=X(1)*CS0
               P5(2)=X(2)*CS0
               P5(3)=X(3)*CS0
               P5(4)=X(4)*CS0
               P5(5)=X(5)**2*CS0
               
             END IF

C-------- end of dipole-allowed -------------------------------
C--------------------------------------------------------------

c ===============================================================

C------------ dipole-forbidden and spin-forbidden -------------
c             ELSE IF(INDEX.EQ.2) THEN



C--------------------Initial guess---------------------------

              IME = 16             

c      IF (ICHECK.EQ.0) THEN
               
               IF (NRUN.EQ.1) THEN
                 X(1)=0.9D0
                 X(2)=0.2D0
                 X(3)=0.1D0
                 X(4)=0.2D0
               ELSE
                 DO KB = 1,4
                   CALL DO_RAND(HARVEST)
                   X(KB) = HARVEST
                 ENDDO
               ENDIF
               
               X(5)=0.12D0

               ICHECK = 0

222            CONTINUE

               CALL LMDIF1(FCN_16,M,N,X,FVEC,TOL,INFO,IWA,WA,LWA)

               SIGMA = DO_SIGMA(FVEC,M)

c               SIGMA=0.D0
c               DO J=1,M
c                 SIGMA = SIGMA + FVEC(J)**2!*CS(J)
c               ENDDO

c               SIGMA = DSQRT(SIGMA/M)

c			   IF (M.GT.5) SIGMA = SIGMA/(M-5.)

               IF (SIGMA.GT.vari) THEN
                 
                 ICHECK = ICHECK + 1
                 IF (ICHECK.EQ.MAXCHK) GOTO 223
c                 print *,'New run: ICHECK = ',ICHECK

c ---- now rotate X's before next attempt

                 DO KL = 1,5
                   X(KL) = DSQRT(DABS(X(KL)/KL))
                 ENDDO

                 GOTO 222
               ENDIF

223            P(1)=X(1)**2*CS0
               P(2)=X(2)**2*CS0
               P(3)=X(3)**2*CS0
               P(4)=X(4)**2*CS0
               P(5)=X(5)**2

c               if (ICHECK.EQ.MAXCHK) print *,filename
c             END IF
             
C=================Output of the results========================

            SIGMA = DO_SIGMA(FVEC,M)

c5528           SIGMA=0.
c               DO J=1,M
c                 SIGMA = SIGMA + FVEC(J)**2!*CS(J)
c               ENDDO
c               SIGMA = DSQRT(SIGMA/M)
c			   IF (M.GT.5) SIGMA = SIGMA/(M-5.)

c                PRINT *," Sigma = ",SIGMA
c                 print 118,INFO,ICHECK,SIGMA*100.
118    FORMAT('info: ',i2,'   ICHECK: ',i5,'     SIGMA: ',F10.3,'%')

               SIXTH = 0.D0
               IF (IME.GE.15) SIXTH = EX(M)
               
             IF (SIGMA.GE.vari*3.d0) THEN
             ! this is a bad fit
                WRITE(12,FMT=92)NLOW,NUPP,INDEX,SIGMA
c                WRITE (*,'(I4,A2,I4,2X,A,I3,A,I3,A,F10.5)') 
c     &                NLOW,"->",NUPP,"    M =",M,
c     &                "    METH =",IME,"    Sigma = ",SIGMA
                WRITE(14,FMT=96) SPCH,NLOW,
     &          NUPP,IME,P(1),P(2),P(3),P(4),P(5),SIXTH,-OSC
c                print*,X
             ELSE  
             ! ok, got a GOOD fit
               GOOD_FIT(NLOW,NUPP) = 1
               N_GOOD = N_GOOD + 1
               
               WRITE(13,FMT=96) SPCH,NLOW,
     &           NUPP,IME,P(1),P(2),P(3),P(4),P(5),SIXTH,-OSC
               IF (INDEX.EQ.0) WRITE(15,FMT=96) SPCH,NLOW,
     &           NUPP,5,P5(1),P5(2),P5(3),P5(4),P5(5),0.,-OSC
               IF (NRUN.GT.1.AND.N_GOOD.EQ.N_TOT) GOTO 57
             END IF

c       print*,ICHECK
c            exit
                icheck = -1
  111        CONTINUE
C--------------------------------------------------------------
          END DO

56        REWIND(10)

       ENDDO


57     CLOSE(12)
       CLOSE(13)
       CLOSE(14)
       CLOSE(10)
C================End of Global Loop============================
c          PRINT *,"Loops made:",COUNT
       PRINT '(I10," good out of ",I10," total")',N_GOOD,N_TOT
 100  FORMAT(A11,I10)
 99   FORMAT(F10.2,E12.3,F13.2)
 97   FORMAT(A12)
 96   FORMAT(" ",I2,"  ",I6," ",I6," ",I2," ",7(1PE11.3))
 95   FORMAT(A8)
 94   FORMAT("inf",I2.1,".dat")
 14   FORMAT("inf",I2.2,".dat")
 93   FORMAT("outp",I2.1,".dat")
 13   FORMAT("outp",I2.2,".dat")
 92   FORMAT(I6,3X,I6,5X,I10,"  ",F8.3)
      STOP
      END


c===============================================

      SUBROUTINE FCN_SASHA (M,N,X,FVEC,IFLAG )
      IMPLICIT REAL*8 (A-H,O-Z)
      DOUBLE PRECISION E(1000),CS(1000),X(N),FVEC(M),XNEW(1000)
      COMMON /BLOCKA/ E,CS, XNEW
      
      DO 10 J=1,M
c         FVEC(J)=DABS((X(1)/E(J) + X(2)/E(J)**2 + X(3)/E(J)**3 
c     &   + X(4)/E(J)**4 +X(5)**2*LOG(E(J))
c     &   /E(J) - CS(J))/CS(J))
         FVEC(J)=DABS((X(1) + X(2)/E(J) + X(3)/E(J)**2 
     &   + X(4)/E(J)**3 +X(5)**2*DLOG(E(J))
     &   - CS(J))/CS(J))
  10  CONTINUE
      RETURN
      END

      SUBROUTINE FCN_15 (M,N,X,FVEC,IFLAG )
      DOUBLE PRECISION E(1000),CS(1000),X(N),FVEC(M),SQ,
     &     X12,X22,X32,X42,X52, DUMMY(1000)
      COMMON /BLOCKA/ DUMMY,CS, E
      
      X12 = X(1)*X(1)
      X22 = X(2)*X(2)
      X32 = X(3)*X(3)
      X42 = X(4)*X(4)
      X52 = X(5)*X(5)
      
      FVEC = 0.D0
      SQ = 0.D0
      
      DO 10 J=1,M
         FVEC(J)=DABS(
     &           (X12/(E(J)*E(J)+X52) + 
     &            X22/((E(J)-0.333D0)**2+X52) +
     &            X32/((E(J)-0.666D0)**2+X52) +
     &            X42/((E(J)-1.000D0)**2+X52)  
     &            - CS(J))
     &           /CS(J))
c     	print*,'j=',j,fvec(j)
c        SQ = SQ + FVEC(J)*FVEC(J)
  10  CONTINUE

c      print '(A4,F10.4)','dsq=',dsqrt(sq)/M
c  			stop
      RETURN
      END

      SUBROUTINE FCN_16 (M,N,X,FVEC,IFLAG )
      DOUBLE PRECISION E(1000),CS(1000),X(N),FVEC(M),SQ,
     &     X12,X22,X32,X42,X52, DUMMY(1000)
      COMMON /BLOCKA/ DUMMY,CS, E
      
      X12 = X(1)*X(1)
      X22 = X(2)*X(2)
      X32 = X(3)*X(3)
      X42 = X(4)*X(4)
      X52 = X(5)*X(5)
      
      FVEC = 0.D0
      SQ = 0.D0
      
      DO 10 J=1,M
         FVEC(J)=DABS(
     &           (X12*DEXP(-E(J)*E(J)/X52) + 
     &            X22*DEXP(-(E(J)-0.333D0)**2/X52) +
     &            X32*DEXP(-(E(J)-0.666D0)**2/X52) +
     &            X42*DEXP(-(E(J)-1.000D0)**2/X52)  
     &            - CS(J))
     &           /CS(J))
c     	print*,'j=',j,fvec(j)
c        SQ = SQ + FVEC(J)*FVEC(J)
  10  CONTINUE

c      print '(A4,F10.4)','dsq=',dsqrt(sq)/M
c  			stop
      RETURN
      END

c===============================================

      SUBROUTINE DO_RAND(HARVEST)
      INTEGER time(8),seed(2)        
      REAL*8 HARVEST
      
      call DATE_AND_TIME(values=time)
      seed(1) = time(4) * 
     *       (360000*time(5) + 6000*time(6) + 100*time(7) + time(8))
      
      CALL RANDOM_SEED()
      CALL RANDOM_NUMBER(HARVEST)
      
      RETURN
      END
      
c================================================

      FUNCTION DO_SIGMA(FVEC,M)
      INTEGER M
      REAL*8 DO_SIGMA,FVEC(1000)

      DO_SIGMA=0.D0
      DO J=1,M
        DO_SIGMA = DO_SIGMA + FVEC(J)**2!*CS(J)
      ENDDO

      DO_SIGMA = DSQRT(DO_SIGMA/M)
      
      RETURN
      END

